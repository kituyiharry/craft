(*
    Recursive descent parser
*)
open Effect;;
open Effect.Deep;;
open Token;;
open Resolver;;

let (let*) = Result.bind
let _MaxArgs = 255 (* maximum number of arguments in a function call *)

type linenum = int [@@deriving show]
type colmnum = int [@@deriving show] 

type tokseq = (tokentype * linenum * colmnum) Seq.t 

type _ Effect.t += 
    | Synchronize: tokseq -> tokseq Effect.t
;;

type stage = 
    | Lex 
    | Parse 
    | Eval 
[@@deriving show];;


module ValEnv = Map.Make (String);;

(* order of type declarations deceptively matters in Ocaml!! *)

type craftsrc = {
        prg: source 
    ;   env: craftenv
} and

craftenv = {
        env: lit ValEnv.t [@opaque]
    ;   par: craftenv option
} and

lit = 
    | Eol
    | Nil
    | Bool     of bool
    | Number   of float
    | String   of string
    | VarIdent of string
    | FunImpl  of (int * craftenv * (craftenv -> lit list -> ((lit * craftenv), crafterr) result))

and context = {
      state: decl list 
    ; errs : expr list
    ; resl : Resolver.t
}

and crafterr = 
    | Unmatched    of linenum * colmnum * tokentype
    | BadExp       of expr * (lit option)
    | BadCond      of expr * (lit option)
    | TypeError    of lit  * expr * lit 
    | BadOp        of lit  * expr * lit
    | EndOfSeq     of string 
    | Unexpected   of int * int * tokentype
    | Undefined    of string
    | Incomplete   of string * expr
    | MaxArgs      of linenum * colmnum * string
    | UnCallable   of expr
    | Unimplmnted  of expr
    | ArgMismatch  of string * int * int (* arity mismatch *)
    | ErrGroup     of string * expr list (* many errors at once *)
    | ScopeError   of linenum * colmnum * string
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
    | Call      of expr list * string * int 

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
    | Ret   of lit (* return value *)

and branch = 
    | If of (expr * decl * decl option)

and decl = 
    | VarDecl of (string * expr)
    | Stmt    of stmt
    | Block   of decl list
    | Branch  of branch
    | Loop    of loop
    | FunDecl of (string * lit list * int * decl)
    | Return  of expr

and loopinit = 
    | LoopDecl of (string * expr)
    | LoopStmt of expr

and loop = 
    | While of (expr * decl)
    | For   of (loopinit * expr * expr * decl)

and source = 
    | Program of context

[@@deriving show];;

(* the interpreter function *)
type interp = (craftenv -> expr -> (lit * craftenv, crafterr) result)

let rec _program resl tseq = 

    match Seq.uncons tseq with
    | Some(((PRINT), _, _), tseq') -> 
        (* print "stuff" *)
        _printstmt resl tseq'
    | Some(((PRINTLN), _, _), tseq') -> 
        (* printl "stuff with newline" *)
        _printlnstmt resl tseq'
    | Some(((VAR), _, _), tseq') -> 
        (* var x = 100; *)
        _vardecl resl tseq'
    | Some (((LEFT_BRACE), l, c), tseq') -> 
        (* { /* more stuff in braces */ } *)
        _blockstmts resl (l, c) [] tseq'
    | Some (((IF), l, c), tseq') -> 
        (* if (cond) { //more stuff in optional braces } *)
        _ifstmts resl (l, c) tseq'
    | Some (((WHILE), l, c), tseq') -> 
        (* while (cond) { /* do stuff */ } *)
        _whilestmt resl (l, c) tseq'
    | Some (((FOR), l, c), tseq') -> 
        (* for (var i = 0; i < max; i = i + 1) { /* stuff */ } *)
        _forstmt resl (l, c) tseq'
    | Some ((FUN, l, c), fncseq') ->
        (* fun name (...) { /* do stuff */ } *)
        _funcblock resl (l, c) fncseq'
    | Some ((IDENTIFIER _ident, _, _), _) ->
        (* x = something; *)
        _assign resl tseq
    | Some ((RETURN, _, _), rseq') ->
        (* return something; *)
        _retstmt resl rseq'
    | _ -> 
        (* 1 + 1 - (2 * 3 / 4) *)
        _express resl tseq

and _retstmt _resl rseq = 
    let* (exp, _resl, rem) = _expression _resl rseq in
    Ok ((Return exp), _resl, rem)

and _funcblock _resl (l, c) fncseq = 

    match Seq.uncons fncseq with
    | Some ((IDENTIFIER name, l, c), more) -> 
        (match Seq.uncons more with
        | Some ((LEFT_PAREN, l, c), more') -> 
            let* exp, _resl, rem = _callexpr _resl (l, c) name more' in
            (match exp with
                | Call (exl, name, arty) -> 
                    let* exl' = List.fold_left (fun acc ac -> 
                        (match acc with 
                            | Ok r ->  
                                (match ac with 
                                    | Literal (VarIdent _ as v) -> (Ok (v :: r))
                                    | e ->
                                        Error (Unhandled (Parse, BadExp (e, None)), _resl, rem)
                                 )
                            | e    -> e
                        )
                    ) (Ok []) exl in
                    let* (blck, _resl, left) = _program _resl rem in
                    Ok (FunDecl (name, exl', arty, blck), _resl, left)
                | e -> 
                    (* likely unreachable *)
                    Error (Unhandled (Parse, (BadExp (e, None))), _resl, fncseq)
            )
        | _ ->
            Error (Unhandled (Parse, (Unexpected (l, c, LEFT_PAREN))), _resl, fncseq)
        )
    | _ -> 
        Error (Unhandled (Parse, (Unexpected (l, c, LEFT_PAREN))), _resl, fncseq)

and _forstmt _resl (l, c) fseq =

    (* NB: can also be desugared into a while loop  *)
    match Seq.uncons fseq with
    | Some ((LEFT_PAREN, l, c), more) -> 
        let* (exp, _resl, more') = _program _resl more in
        (match exp with
            | VarDecl dcl -> 
                (match Seq.uncons more' with
                    | Some ((SEMICOLON, l, c), rem) -> 
                        let* (exp, _resl, rem') = _expression _resl rem in
                        (match Seq.uncons rem' with 
                            | Some ((SEMICOLON, _l, _c), rem'') -> 
                                let* (asg, _resl, rem''') = _assignment _resl rem'' in
                                (match Seq.uncons rem''' with
                                    | Some ((RIGHT_PAREN, _l, _c), fin) -> 
                                        let* (loopblk, _resl, fin) = _program _resl fin in
                                        let _forstmt = (For ((LoopDecl dcl), exp, asg, loopblk)) in
                                        Ok (Loop _forstmt, _resl, fin)
                                    | _ -> 
                                        Error (Unhandled (Parse, (Unexpected (l, c, SEMICOLON))), _resl, fseq)
                                )
                            | _ ->
                                Error (Unhandled (Parse, (Unexpected (l, c, SEMICOLON))), _resl, fseq)
                        )
                    | _ ->
                        Error (Unhandled (Parse, (Unexpected (l, c, LEFT_PAREN))), _resl, fseq)
                )
            | Stmt (Raw (Eval evl)) ->
                (match Seq.uncons more' with
                    | Some ((SEMICOLON, l, c), rem) -> 
                        let* (exp, _resl, rem') = _expression _resl rem in
                        (match Seq.uncons rem' with 
                            | Some ((SEMICOLON, _l, _c), rem'') -> 
                                let* (asg, _resl, rem''') = _assignment _resl rem'' in
                                (match Seq.uncons rem''' with
                                    | Some ((RIGHT_PAREN, _l, _c), fin) -> 
                                        let* (loopblk, _resl, fin) = _program _resl fin in
                                        let _forstmt = (For ((LoopStmt evl), exp, asg, loopblk)) in
                                        Ok (Loop _forstmt, _resl, fin)
                                    | _ -> 
                                        Error (Unhandled (Parse, (Unexpected (l, c, SEMICOLON))), _resl, fseq)
                                )
                            | _ ->
                                Error (Unhandled (Parse, (Unexpected (l, c, SEMICOLON))), _resl, fseq)
                        )
                    | _ ->
                        Error (Unhandled (Parse, (Unexpected (l, c, LEFT_PAREN))), _resl, fseq)
                )
            | _ -> 
                Error (Unhandled (Parse, (Unexpected (l, c, LEFT_PAREN))), _resl, fseq)
        )
    | Some ((_, l,c), _) -> 
        Error (Unhandled (Parse, (Unexpected (l, c, LEFT_PAREN))), _resl, fseq)
    | _ -> 
        Error (Unhandled (Parse, (Unexpected (l, c, LEFT_PAREN))), _resl, fseq)

and _whilestmt _resl (l, c) ifseq = 

    match Seq.uncons ifseq with
    | Some ((LEFT_PAREN, l, c), more) -> 
        let* (exp, _resl, more') = _expression _resl more in
        (match Seq.uncons more' with
        | Some ((RIGHT_PAREN, _, _), left) -> 
            let* (loopbr, _resl, left') = _program _resl left in 
            Ok (Loop (While (exp, loopbr)), _resl, left')
        | _ ->
            Error (Unhandled (Parse, (Unmatched (l, c, RIGHT_PAREN))), _resl, ifseq)
        )
    | Some ((_, l,c), _) -> 
        Error (Unhandled (Parse, (Unexpected (l, c, LEFT_PAREN))), _resl, ifseq)
    | _ -> 
        Error (Unhandled (Parse, (Unexpected (l, c, LEFT_PAREN))), _resl, ifseq)

and _ifstmts _resl (l, c) ifseq = 

    match Seq.uncons ifseq with
    | Some ((LEFT_PAREN, l, c), more) -> 
        let* (exp, _resl, more') = _expression _resl more in
        (match Seq.uncons more' with
        | Some ((RIGHT_PAREN, _, _), left) -> 
            let* (thenbr, _resl, left') = _program _resl left in 
            (match Seq.uncons left' with
                | Some ((ELSE, _, _), more) -> 
                    let* (elsebr, _resl, left'') = _program _resl more in 
                    Ok (Branch (If (exp, thenbr, Some elsebr)), _resl, left'')
                | _ -> 
                    Ok (Branch (If (exp, thenbr, None)), _resl, left')
            )
        | _ ->
            Error (Unhandled (Parse, (Unexpected (l, c, RIGHT_PAREN))), _resl, ifseq)
        )
    | Some ((_, l,c), _) -> 
        Error (Unhandled (Parse, (Unexpected (l, c, LEFT_PAREN))), _resl, ifseq)
    | _ -> 
        Error (Unhandled (Parse, (Unexpected (l, c, LEFT_PAREN))), _resl, ifseq)

and _blockstmts _resl (l, c) stmts bseq =
    let _resl = Resolver.begin_scope _resl in
    let rec check _resl (l, c) stmts bseq =
        match Seq.uncons bseq with
        | Some ((RIGHT_BRACE, _, _), more) -> 
            let _resl = Resolver.end_scope _resl in
            Ok (Block (List.rev stmts), _resl, more)
        | Some ((SEMICOLON, l, c), more) -> 
            check _resl (l, c) stmts more
        | Some ((_, _, _), _) ->
            let* (exp, _resl, more') = _program _resl bseq in 
            (match Seq.uncons more' with
                | Some ((_, l, c), _) -> 
                    check _resl (l, c) (exp :: stmts) more'
                | _ -> 
                    check _resl (l, c) (exp :: stmts) more'
            )
        | _ -> 
            Error (Unhandled (Parse, (Unmatched (l, c, RIGHT_BRACE))), _resl, bseq)
    in check _resl (l, c) stmts bseq

and _assign _resl aseq = 
    let* (ast', _resl, ts') = _assignment _resl aseq in
    Ok ((Stmt (Raw (Eval ast'))), _resl, ts')

and _assignment _resl aseq = 
    let* (exp, _resl, rem) = _expression _resl aseq in

    (match Seq.uncons rem with
        | Some(((EQUAL), _, _), aseq') -> 
            let* (exp', _resl, rem') = _assignment _resl aseq' in 
            (match exp with
                | Literal (VarIdent n) -> 
                    Ok ((Assign (n, exp')), _resl, rem')
                | Literal _n as l -> 
                    Ok (l, _resl, rem')
                | _ -> 
                    Error (Unhandled (Parse, Incomplete ("invalid assignment", exp')), _resl, rem'))
        | _ -> 
            Ok (exp, _resl, rem)
    )

and _express _resl eseq = 
    let* (ast', _resl, ts') = _expression _resl eseq in 
    Ok ((Stmt (Raw (Eval ast'))), _resl, ts')

and _printstmt _resl pseq =
    let* (ast', _resl, ts') = _expression _resl pseq in 
    Ok (Stmt (Side (Effect (Print ast'))), _resl, ts')

and _printlnstmt _resl pseq =
    let* (ast', _resl, ts') = _expression _resl pseq in 
    Ok (Stmt (Side (Effect (Println ast'))), _resl, ts')

and _vardecl _resl vseq = 

    match Seq.uncons vseq with
    | Some(((IDENTIFIER ident), _, _), tseq') -> 
        let _resl = Resolver.declare _resl ident in
        (match Seq.uncons tseq' with
            | Some(((EQUAL), _, _), tseq'') -> 
                let* (ast, _resl, ts) = _expression _resl tseq'' in 
                let _ = Resolver.define _resl ident in
                Ok (VarDecl (ident, ast), _resl, ts)
            | Some ((SEMICOLON, _l, _c), tseq'') -> 
                let _ = Resolver.define _resl ident in
                Ok (VarDecl (ident, (Literal Nil)), _resl, tseq'')
            | Some ((t, l, c), tseq'') -> 
                Error ((Unhandled (Parse, (Unexpected (l, c, t)))), _resl, tseq'')
            | _ -> 
                let e = "expected var identifier expr" in
                Error ((Unhandled (Parse, (EndOfSeq e))), _resl, tseq')
        )
    | Some((p , l, c), _tseq') -> 
        Error ((Unhandled (Parse, Unexpected (l, c, p))), _resl, vseq)
    | _ -> 
        let e = "expected identifier token" in
        Error ((Unhandled (Parse, EndOfSeq e)), _resl, vseq)

and _expression _resl exseq' = 
    _logical _resl exseq'

and _logical _resl lseq' =
    _logicalor _resl lseq'

and _logicalor _resl oseq' =

    let* (lexpr, _resl, tseq'') = _logicaland _resl oseq' in 

    let rec check _resl l_expr ts = 
        match Seq.uncons ts with
        | Some ((p, _l, _c), r) ->
            (match p with
                | OR  -> 
                    let* r_expr, _resl, _ts' = _logicaland _resl r in
                    let l_expr' = (Binary (l_expr, (Logic Or), r_expr)) 
                    in check _resl l_expr' _ts' 
                | _ -> 
                    Ok (l_expr, _resl, ts)
            )
        | None ->
            Ok (l_expr, _resl, ts)
    in check _resl lexpr tseq'' 


and _logicaland _resl aseq' =

    let* (lexpr, _resl, tseq'') = _equality _resl aseq' in 

    let rec check _resl l_expr ts = 
        match Seq.uncons ts with
        | Some ((p, _l, _c), r) ->
            (match p with
                | AND  -> 
                    let* r_expr, _resl, _ts' = _equality _resl r in
                    let l_expr' = (Binary (l_expr, (Logic And), r_expr)) 
                    in check _resl l_expr' _ts' 
                | _ -> 
                    Ok (l_expr, _resl, ts)
            )
        | None ->
            Ok (l_expr, _resl, ts)

    in check _resl lexpr tseq'' 

and _equality _resl tseq' = 
    let* (lexpr, _resl, tseq'') = _comp _resl tseq' in 

    let rec check _resl l_expr ts = 
        match Seq.uncons ts with
        | Some ((p, _l, _c), r) ->
            (match p with
                | BANG_EQUAL  -> 
                    let* r_expr, _resl, _ts' = _comp _resl r in
                    let l_expr' = (Binary (l_expr, (Operator NotEq), r_expr)) 
                    in check _resl l_expr' _ts' 
                | EQUAL_EQUAL -> 
                    let* r_expr, _resl, _ts' = _comp _resl r in
                    let l_expr' = (Binary (l_expr, (Operator Eq), r_expr)) 
                    in check _resl l_expr' _ts' 
                | _ -> 
                    Ok (l_expr, _resl, ts)
            )
        | None ->
            Ok (l_expr, _resl, ts)
    in check _resl lexpr tseq'' 

and _comp _resl compseq  = 
    let* (lexpr, _resl, compseq') = _term _resl compseq in

    let rec check _resl l_expr ts = 
        match Seq.uncons ts with
        | Some ((p, _l, _c), r) ->
            (match p with
                | GREATER  -> 
                    let* r_expr, _resl, _ts' = _term _resl r in
                    let lexpr' = (Binary (l_expr, (Compr Greater), r_expr)) in 
                    check _resl lexpr' _ts'
                | GREATER_EQUAL -> 
                    let* r_expr, _resl, _ts' = _term _resl r in
                    let lexpr' = (Binary (l_expr, (Compr GreaterEq), r_expr)) in
                    check _resl lexpr' _ts'
                | LESS -> 
                    let* r_expr, _resl, _ts' = _term _resl r in
                    let lexpr' = (Binary (l_expr, (Compr Lesser), r_expr)) in 
                    check _resl lexpr' _ts'
                | LESS_EQUAL -> 
                    let* r_expr, _resl, _ts' = _term _resl r in
                    let lexpr' = (Binary (l_expr, (Compr LesserEq), r_expr)) in
                    check _resl lexpr' _ts'
                | _ -> 
                    Ok (l_expr, _resl, ts)
            )
        | None ->
            Ok (l_expr, _resl, ts)
    in check _resl lexpr compseq' 

and _term _resl termseq =  
    let* (lexpr, _resl, termseq') = _factor _resl termseq in

    let rec check _resl l_expr ts = 
        match Seq.uncons ts with
        | Some ((p, _l, _c), r) ->
            (match p with
                | MINUS  -> 
                    let* r_expr, _resl, _ts' = _term _resl r in
                    let lexpr' = (Binary (l_expr, (Term Sub), r_expr)) in
                    check _resl lexpr' _ts'
                | PLUS -> 
                    let* r_expr, _resl, _ts' = _term _resl r in
                    let lexpr' = (Binary (l_expr, (Term Add), r_expr)) in
                    check _resl lexpr' _ts'
                | _ -> 
                    Ok (l_expr, _resl, ts)
            )
        | None ->
            Ok (l_expr, _resl, ts)
    in check _resl lexpr termseq' 

and _factor _resl facseq =
    let* (lexpr, _resl, facseq') = _unary _resl facseq in

    let rec check _resl l_expr ts = 
        match Seq.uncons ts with
        | Some ((p, _l, _c), r) ->
            (match p with
                | SLASH  -> 
                    let* r_expr, _resl, _ts' = _term _resl r in
                    let lexpr' = (Binary (l_expr, (Factor Div), r_expr)) in
                    check _resl lexpr' _ts'
                | STAR -> 
                    let* r_expr, _resl, _ts' = _term _resl r in
                    let lexpr' = (Binary (l_expr, (Factor Mul), r_expr)) in 
                    check _resl lexpr' _ts'
                | _ -> 
                    Ok (l_expr, _resl, ts)
            )
        | None ->
            Ok (l_expr, _resl, ts)
    in check _resl lexpr facseq' 

and _unary _resl useq = 

    (match Seq.uncons useq with
    | Some ((p, _l, _c), r) ->
        (match p with
            | BANG  -> 
                let* r_expr, _resl, _ts' = _unary _resl r in
                Ok (Unary (Invert, r_expr), _resl, _ts')
            | MINUS -> 
                let* r_expr, _resl, _ts' = _unary _resl r in
                Ok (Unary (Negate, r_expr), _resl, _ts')
            | _ -> 
                call _resl useq
        )
    | None ->
        Ok (Literal Eol, _resl, useq)
    ) 

and call _resl cseq =  
    let* (ex, _resl, cseq') = primary _resl cseq in

    (match Seq.uncons cseq' with
        | Some ((LEFT_PAREN, _l, _c), r) ->
            (match ex with
            | Literal VarIdent name ->  
                _callexpr _resl (_l, _c) name r
            | _ -> 
                Error (Unhandled (Parse, UnCallable ex), _resl, r)
            )
        | _ ->  
            Ok (ex, _resl, cseq')
    )


and _callexpr _resl (l, c) ex ce = 
    let args = [] in
    let size = 0 in
    let rec check _resl size args cs = 
        if size >= _MaxArgs then 
            Error (Unhandled (Parse, MaxArgs (l, c, ex)), _resl, cs)
        else
        (match Seq.uncons cs with
            | Some ((RIGHT_PAREN, _l, _c), r) ->
                Ok (Call (args, ex, size), _resl, r)
            | _ -> 
                let* (ex', _resl, rem) = _expression _resl cs in
                (match Seq.uncons rem with
                    | Some ((COMMA, _l, _c), r) ->
                        check _resl (size + 1) (ex' :: args) r
                    | Some ((RIGHT_PAREN, _l, _c), r) ->
                        Ok (Call (ex' :: args, ex, (size + 1)), _resl,  r)
                    | _ -> 
                        Ok (Call (ex' :: args, ex, (size + 1)), _resl, rem)
                ) 
        )
    in check _resl size args ce

and primary _resl pseq = 

    match Seq.uncons pseq with
    | Some ((p, l', c'), r) ->
        (match p with
            | FALSE        -> Ok (Literal (Bool false), _resl, r)
            | TRUE         -> Ok (Literal (Bool true) , _resl, r)
            | NUMBER f     -> Ok (Literal (Number f)  , _resl, r)
            | STRING s     -> Ok (Literal (String s)  , _resl, r)
            | IDENTIFIER i -> Ok (Literal (VarIdent i), _resl, r)
            | LEFT_PAREN ->
                let* expr', _resl, r' = _expression _resl r in
                (match Seq.uncons r' with
                    | Some ((RIGHT_PAREN, _l, _c), r'') -> 
                        Ok (Grouping expr', _resl, r'')
                    | Some ((_, l, c), _r'') -> 
                        let r''' = perform (Synchronize r') in 
                        Error (Unhandled (Parse, Unmatched (l, c, RIGHT_PAREN)),  _resl, r''')
                    | _ -> 
                        let r''' = perform (Synchronize r') in 
                        Error (Unhandled (Parse, Unmatched (l', c', RIGHT_PAREN)), _resl, r''')
                )
            | t -> 
                Error (Unhandled (Parse, Unexpected (l', c', t)), _resl, r)
        )
    | None ->
        Error ((Literal Eol), _resl, pseq)
;;

let parse tseq = 
    let res = Resolver.empty in
    let rec _parse s ts =
        match  _program s.resl ts with
        | Ok (stmt, resl, more) ->
            if Seq.is_empty more then
                let _ = Format.printf "%s\n" (Resolver.show resl) in
                Ok (Program ({ s with
                    state = (List.rev (stmt :: s.state)); 
                    resl  = s.resl;
                }))
            else
                (match Seq.uncons more with
                    | Some ((SEMICOLON, _l', _c'), more') -> 
                        _parse { s with state=(stmt :: s.state); resl=resl } more'
                    | _ ->
                        _parse { s with state=(stmt :: s.state) } more)
        | Error (e, resl, more) -> 
            _parse { s with errs=(e :: s.errs); resl=resl } more
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
    in _parse { state=[]; errs=[]; resl=res; } tseq
;;

