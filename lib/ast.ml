(*
    Recursive descent parser
*)
open Effect;;
open Effect.Deep;;
open Token;;

let (let*) = Result.bind

type linenum = int 
type colmnum = int 

type tokseq = (tokentype * linenum * colmnum) Seq.t 

type _ Effect.t += 
    | Synchronize: tokseq -> tokseq Effect.t
;;

type stage = 
    | Lex 
    | Parse 
    | Eval 
[@@deriving show];;

type lit = 
    | Eol
    | Nil
    | Bool     of bool
    | Number   of float
    | String   of string
    | VarIdent of string

and context = {
      state: decl list 
    ; errs : expr list
}

and crafterr = 
    | Unmatched    of int * int * tokentype
    | BadExp       of expr * (lit option)
    | BadCond      of expr * (lit option)
    | TypeError    of lit * expr * lit 
    | BadOp        of lit * expr * lit
    | EndOfSeq     of string 
    | Unexpected   of int * int * tokentype
    | Undefined    of string
    | Incomplete   of string * expr
    | Unterminated

and unary = 
    | Negate 
    | Invert 

and factor = 
    | Div 
    | Mul 

and term = 
    | Sub 
    | Add 

and comparison = 
    | Greater 
    | GreaterEq 
    | Lesser 
    | LesserEq 

and equality = 
    | Eq 
    | NotEq 

and logical = 
    | And
    | Or

and expr =
    | Literal   of lit
    | Factor    of factor
    | Term      of term
    | Compr     of comparison
    | Operator  of equality
    | Unary     of unary * expr
    | Binary    of expr * expr * expr
    | Grouping  of expr 
    | Logic     of logical
    | Assign    of string * expr
    | Unhandled of stage * crafterr (* line-ast token line col *)

and builtin = 
    | Print of expr
    | Println of expr

and exprst = 
    | Eval of expr

and apply = 
    | Effect of builtin

and stmt  = 
    | Raw   of exprst
    | Side  of apply

and branch = 
    | If of (expr * decl * decl option)

and decl = 
    | VarDecl of (string * expr)
    | Stmt    of stmt
    | Block   of decl list
    | Branch  of branch
    | Loop    of loop

and loopinit = 
    | LoopDecl of (string * expr)
    | LoopStmt of expr

and loop = 
    | While of (expr * decl)
    | For   of (loopinit * expr * expr * decl)

and source = 
    | Program of context

[@@deriving show];;

let rec _program tseq = 

    match Seq.uncons tseq with
    | Some(((PRINT), _, _), tseq') -> 
        _printstmt tseq'
    | Some(((PRINTLN), _, _), tseq') -> 
        _printlnstmt tseq'
    | Some(((VAR), _, _), tseq') -> 
        _vardecl tseq'
    | Some (((LEFT_BRACE), l, c), tseq') -> 
        _blockstmts (l, c) [] tseq'
    | Some (((IF), l, c), tseq') -> 
        _ifstmts (l, c) tseq'
    | Some (((WHILE), l, c), tseq') -> 
        _whilestmt (l, c) tseq'
    | Some (((FOR), l, c), tseq') -> 
        _forstmt (l, c) tseq'
    | Some ((IDENTIFIER _ident, _, _), _) ->
        _assign tseq
    | _ -> 
        _express tseq

and _forstmt (l, c) fseq =

    match Seq.uncons fseq with
    | Some ((LEFT_PAREN, l, c), more) -> 
        let* (exp, more') = _program more in
        (match exp with
            | VarDecl dcl -> 
                (match Seq.uncons more' with
                    | Some ((SEMICOLON, l, c), rem) -> 
                        let* (exp, rem') = _expression rem in
                        (match Seq.uncons rem' with 
                            | Some ((SEMICOLON, _l, _c), rem'') -> 
                                let* (asg, rem''') = _assignment rem'' in
                                (match Seq.uncons rem''' with
                                    | Some ((RIGHT_PAREN, _l, _c), fin) -> 
                                        let* (loopblk, fin) = _program fin in
                                        let _forstmt = (For ((LoopDecl dcl), exp, asg, loopblk)) in
                                        Ok (Loop _forstmt, fin)
                                    | _ -> 
                                        Error (Unhandled (Parse, (Unexpected (l, c, SEMICOLON))), fseq)
                                )
                            | _ ->
                                Error (Unhandled (Parse, (Unexpected (l, c, SEMICOLON))), fseq)
                        )
                    | _ ->
                        Error (Unhandled (Parse, (Unexpected (l, c, LEFT_PAREN))), fseq)
                )
            | Stmt (Raw (Eval evl)) ->
                (match Seq.uncons more' with
                    | Some ((SEMICOLON, l, c), rem) -> 
                        let* (exp, rem') = _expression rem in
                        (match Seq.uncons rem' with 
                            | Some ((SEMICOLON, _l, _c), rem'') -> 
                                let* (asg, rem''') = _assignment rem'' in
                                (match Seq.uncons rem''' with
                                    | Some ((RIGHT_PAREN, _l, _c), fin) -> 
                                        let* (loopblk, fin) = _program fin in
                                        let _forstmt = (For ((LoopStmt evl), exp, asg, loopblk)) in
                                        Ok (Loop _forstmt, fin)
                                    | _ -> 
                                        Error (Unhandled (Parse, (Unexpected (l, c, SEMICOLON))), fseq)
                                )
                            | _ ->
                                Error (Unhandled (Parse, (Unexpected (l, c, SEMICOLON))), fseq)
                        )
                    | _ ->
                        Error (Unhandled (Parse, (Unexpected (l, c, LEFT_PAREN))), fseq)
                )
            | _ -> 
                Error (Unhandled (Parse, (Unexpected (l, c, LEFT_PAREN))), fseq)
        )
    | Some ((_, l,c), _) -> 
        Error (Unhandled (Parse, (Unexpected (l, c, LEFT_PAREN))), fseq)
    | _ -> 
        Error (Unhandled (Parse, (Unexpected (l, c, LEFT_PAREN))), fseq)

and _whilestmt (l, c) ifseq = 

    match Seq.uncons ifseq with
    | Some ((LEFT_PAREN, l, c), more) -> 
        let* (exp, more') = _expression more in
        (match Seq.uncons more' with
        | Some ((RIGHT_PAREN, _, _), left) -> 
            let* (loopbr, left') = _program left in 
            Ok (Loop (While (exp, loopbr)), left')
        | _ ->
            Error (Unhandled (Parse, (Unmatched (l, c, RIGHT_PAREN))), ifseq)
        )
    | Some ((_, l,c), _) -> 
        Error (Unhandled (Parse, (Unexpected (l, c, LEFT_PAREN))), ifseq)
    | _ -> 
        Error (Unhandled (Parse, (Unexpected (l, c, LEFT_PAREN))), ifseq)

and _ifstmts (l, c) ifseq = 

    match Seq.uncons ifseq with
    | Some ((LEFT_PAREN, l, c), more) -> 
        let* (exp, more') = _expression more in
        (match Seq.uncons more' with
        | Some ((RIGHT_PAREN, _, _), left) -> 
            let* (thenbr, left') = _program left in 
            (match Seq.uncons left' with
                | Some ((ELSE, _, _), more) -> 
                    let* (elsebr, left'') = _program more in 
                    Ok (Branch (If (exp, thenbr, Some elsebr)), left'')
                | _ -> 
                    Ok (Branch (If (exp, thenbr, None)), left')
            )
        | _ ->
            Error (Unhandled (Parse, (Unexpected (l, c, RIGHT_PAREN))), ifseq)
        )
    | Some ((_, l,c), _) -> 
        Error (Unhandled (Parse, (Unexpected (l, c, LEFT_PAREN))), ifseq)
    | _ -> 
        Error (Unhandled (Parse, (Unexpected (l, c, LEFT_PAREN))), ifseq)

and _blockstmts (l, c) stmts bseq =
    match Seq.uncons bseq with
    | Some ((RIGHT_BRACE, _, _), more) -> 
        Ok (Block (List.rev stmts), more)
    | Some ((SEMICOLON, l, c), more) -> 
        _blockstmts (l, c) stmts more
    | Some ((_, _, _), _) ->
        let* (exp, more') = _program bseq in 
        (match Seq.uncons more' with
            | Some ((_, l, c), _) -> 
                _blockstmts (l, c) (exp :: stmts) more'
            | _ -> 
                _blockstmts (l, c) (exp :: stmts) more'
        )
    | _ -> 
        Error (Unhandled (Parse, (Unmatched (l, c, RIGHT_BRACE))), bseq)

and _assign aseq = 
    let* (ast', ts') = _assignment aseq in
    Ok ((Stmt (Raw (Eval ast'))), ts')

and _assignment aseq = 
    let* (exp, rem) = _expression aseq in

    (match Seq.uncons rem with
        | Some(((EQUAL), _, _), aseq') -> 
            let* (exp', rem') = _assignment aseq' in 
            (match exp with
                | Literal (VarIdent n) -> 
                    Ok ((Assign (n, exp')), rem')
                | Literal _n as l -> 
                    Ok (l, rem')
                | _ -> 
                    Error (Unhandled (Parse, Incomplete ("invalid assignment", exp')), rem'))
        | _ -> 
            Ok (exp, rem)
    )

and _express eseq = 
    let* (ast', ts') = _expression eseq in 
    Ok ((Stmt (Raw (Eval ast'))), ts')

and _printstmt pseq =
    let* (ast', ts') = _expression pseq in 
    Ok (Stmt (Side (Effect (Print ast'))), ts')

and _printlnstmt pseq =
    let* (ast', ts') = _expression pseq in 
    Ok (Stmt (Side (Effect (Println ast'))), ts')

and _vardecl vseq = 

    match Seq.uncons vseq with
    | Some(((IDENTIFIER ident), _, _), tseq') -> 
        (match Seq.uncons tseq' with
            | Some(((EQUAL), _, _), tseq'') -> 
                let* (ast, ts) = _expression tseq'' in 
                Ok (VarDecl (ident, ast), ts)
            | Some ((t, l, c), tseq'') -> 
                Error ((Unhandled (Parse, (Unexpected (l, c, t)))), tseq'')
            | _ -> 
                let e = "expected var identifier expr" in
                Error ((Unhandled (Parse, (EndOfSeq e))), tseq')
        )
    | Some((p , l, c), _tseq') -> 
        Error ((Unhandled (Parse, Unexpected (l, c, p))), vseq)
    | _ -> 
        let e = "expected identifier token" in
        Error ((Unhandled (Parse, EndOfSeq e)), vseq)

and _expression exseq' = 
    _logical exseq'

and _logical lseq' =
    _logicalor lseq'

and _logicalor oseq' =

    let* (lexpr, tseq'') = _logicaland oseq' in 

    let rec check l_expr ts = 
        match Seq.uncons ts with
        | Some ((p, _l, _c), r) ->
            (match p with
                | OR  -> 
                    let* r_expr, _ts' = _logicaland r in
                    let l_expr' = (Binary (l_expr, (Logic Or), r_expr)) 
                    in check l_expr' _ts' 
                | _ -> 
                    Ok (l_expr, ts)
            )
        | None ->
            Ok (l_expr, ts)
    in check lexpr tseq'' 


and _logicaland aseq' =

    let* (lexpr, tseq'') = _equality aseq' in 

    let rec check l_expr ts = 
        match Seq.uncons ts with
        | Some ((p, _l, _c), r) ->
            (match p with
                | AND  -> 
                    let* r_expr, _ts' = _equality r in
                    let l_expr' = (Binary (l_expr, (Logic And), r_expr)) 
                    in check l_expr' _ts' 
                | _ -> 
                    Ok (l_expr, ts)
            )
        | None ->
            Ok (l_expr, ts)

    in check lexpr tseq'' 

and _equality tseq' = 
    let* (lexpr, tseq'') = _comp tseq' in 

    let rec check l_expr ts = 
        match Seq.uncons ts with
        | Some ((p, _l, _c), r) ->
            (match p with
                | BANG_EQUAL  -> 
                    let* r_expr, _ts' = _comp r in
                    let l_expr' = (Binary (l_expr, (Operator NotEq), r_expr)) 
                    in check l_expr' _ts' 
                | EQUAL_EQUAL -> 
                    let* r_expr, _ts' = _comp r in
                    let l_expr' = (Binary (l_expr, (Operator Eq), r_expr)) 
                    in check l_expr' _ts' 
                | _ -> 
                    Ok (l_expr, ts)
            )
        | None ->
            Ok (l_expr, ts)
    in check lexpr tseq'' 

and _comp compseq  = 
    let* (lexpr, compseq') = _term compseq in

    let rec check l_expr ts = 
        match Seq.uncons ts with
        | Some ((p, _l, _c), r) ->
            (match p with
                | GREATER  -> 
                    let* r_expr, _ts' = _term r in
                    let lexpr' = (Binary (l_expr, (Compr Greater), r_expr)) in 
                    check lexpr' _ts'
                | GREATER_EQUAL -> 
                    let* r_expr, _ts' = _term r in
                    let lexpr' = (Binary (l_expr, (Compr GreaterEq), r_expr)) in
                    check lexpr' _ts'
                | LESS -> 
                    let* r_expr, _ts' = _term r in
                    let lexpr' = (Binary (l_expr, (Compr Lesser), r_expr)) in 
                    check lexpr' _ts'
                | LESS_EQUAL -> 
                    let* r_expr, _ts' = _term r in
                    let lexpr' = (Binary (l_expr, (Compr LesserEq), r_expr)) in
                    check lexpr' _ts'
                | _ -> 
                    Ok (l_expr, ts)
            )
        | None ->
            Ok (l_expr, ts)
    in check lexpr compseq' 

and _term termseq =  
    let* (lexpr, termseq') = _factor termseq in

    let rec check l_expr ts = 
        match Seq.uncons ts with
        | Some ((p, _l, _c), r) ->
            (match p with
                | MINUS  -> 
                    let* r_expr, _ts' = _term r in
                    let lexpr' = (Binary (l_expr, (Term Sub), r_expr)) in
                    check lexpr' _ts'
                | PLUS -> 
                    let* r_expr, _ts' = _term r in
                    let lexpr' = (Binary (l_expr, (Term Add), r_expr)) in
                    check lexpr' _ts'
                | _ -> 
                    Ok (l_expr, ts)
            )
        | None ->
            Ok (l_expr, ts)
    in check lexpr termseq' 

and _factor facseq =
    let* (lexpr, facseq') = _unary facseq in

    let rec check l_expr ts = 
        match Seq.uncons ts with
        | Some ((p, _l, _c), r) ->
            (match p with
                | SLASH  -> 
                    let* r_expr, _ts' = _term r in
                    let lexpr' = (Binary (l_expr, (Factor Div), r_expr)) in
                    check lexpr' _ts'
                | STAR -> 
                    let* r_expr, _ts' = _term r in
                    let lexpr' = (Binary (l_expr, (Factor Mul), r_expr)) in 
                    check lexpr' _ts'
                | _ -> 
                    Ok (l_expr, ts)
            )
        | None ->
            Ok (l_expr, ts)
    in check lexpr facseq' 

and _unary useq = 

    (match Seq.uncons useq with
    | Some ((p, _l, _c), r) ->
        (match p with
            | BANG  -> 
                let* r_expr, _ts' = _unary r in
                Ok (Unary (Invert, r_expr), _ts')
            | MINUS -> 
                let* r_expr, _ts' = _unary r in
                Ok (Unary (Negate, r_expr), _ts')
            | _ -> 
                primary useq
        )
    | None ->
        Ok (Literal Eol, useq)
    ) 

and primary pseq = 

    match Seq.uncons pseq with
    | Some ((p, l', c'), r) ->
        (match p with
            | FALSE        -> Ok (Literal (Bool false), r)
            | TRUE         -> Ok (Literal (Bool true) , r)
            | NUMBER f     -> Ok (Literal (Number f)  , r)
            | STRING s     -> Ok (Literal (String s)  , r)
            | IDENTIFIER i -> Ok (Literal (VarIdent i), r)
            | LEFT_PAREN ->
                let* expr', r' = _expression r in
                (match Seq.uncons r' with
                    | Some ((RIGHT_PAREN, _l, _c), r'') -> 
                        Ok (Grouping expr', r'')
                    | Some ((_, l, c), _r'') -> 
                        let r''' = perform (Synchronize r') in 
                        Error (Unhandled (Parse, Unmatched (l, c, RIGHT_PAREN)), r''')
                    | _ -> 
                        let r''' = perform (Synchronize r') in 
                        Error (Unhandled (Parse, Unmatched (l', c', RIGHT_PAREN)), r''')
                )
            | t -> 
                Error (Unhandled (Parse, Unexpected (l', c', t)), r)
        )
    | None ->
        Error ((Literal Eol), pseq)
;;

let parse tseq = 
    let rec _parse s ts =
        match  _program ts with
        | Ok (stmt, more) ->
            if Seq.is_empty more then
                Ok (Program ({ s with
                    state = (List.rev (stmt :: s.state))
                }))
            else
                (match Seq.uncons more with
                    | Some ((SEMICOLON, _l', _c'), more') -> 
                        _parse { s with state=(stmt :: s.state) } more'
                    | _ ->
                        _parse { s with state=(stmt :: s.state) } more)
        | Error (e, more) -> 
            _parse { s with errs=(e :: s.errs) } more
        (* failover for parse errors *)
        | effect Synchronize (t), k -> 
            let t' = Seq.drop_while (fun (p, _l, _c) -> 
                match p with
                | SEMICOLON -> false
                | FUN       -> false
                | VAR       -> false
                | FOR       -> false
                | IF        -> false
                | WHILE     -> false
                | PRINT     -> false
                | RETURN    -> false
                | _         -> true
            ) t in
            (match Seq.uncons t' with
                | Some ((SEMICOLON, _, _), r) ->
                    continue k r
                | _ -> 
                    continue k t'
            )
    in _parse { state=[]; errs=[] } tseq
;;

