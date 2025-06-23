(*
    Recursive descent parser
*)
open Effect;;
open Effect.Deep;;
open Token;;

let (let*) = Result.bind

type tokseq = (tokentype * int * int ) Seq.t 

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
      state: declaration list 
    ; errs : expr list
}

and crafterr = 
    | Unmatched    of int * int * expr 
    | BadExp       of expr * (lit option)
    | BadMatch     of lit * expr * lit 
    | BadOp        of lit * expr * lit
    | EndOfSeq     of string 
    | Unexpected   of int * int * tokentype
    | Unterminated
    | Undefined    of string
    | FailedEval   of crafterr

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

and expr =
    | Literal   of lit
    | Factor    of factor
    | Term      of term
    | Compr     of comparison
    | Operator  of equality
    | Unary     of unary * expr
    | Binary    of expr * expr * expr
    | Grouping  of expr 
    | Unhandled of stage * crafterr (* line-ast token line col *)

and builtin = 
    | Print of expr

and exprst = 
    | Eval of expr

and apply = 
    | Effect of builtin

and stmt  = 
    | Raw  of exprst
    | Side of apply

and declaration = 
    | VarDecl of (string * expr)
    | Stmt    of stmt

and source = 
    | Program of context

[@@deriving show];;

let rec _program tseq = 

    match Seq.uncons tseq with
    | Some(((PRINT), _, _), tseq') -> 
        _printstmt tseq'
    | Some(((VAR), _, _), tseq') -> 
        _vardecl tseq'
    | _ -> 
        _express tseq

and _express eseq = 
    let* (ast', ts') = _expression eseq in 
    Ok ((Stmt (Raw (Eval ast'))), ts')

and _printstmt pseq =
    let* (ast', ts') = _expression pseq in 
    Ok (Stmt (Side (Effect (Print ast'))), ts')

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
    _equality exseq'

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
                        Error (Unhandled (Parse, Unmatched (l, c, expr')), r''')
                    | _ -> 
                        let r''' = perform (Synchronize r') in 
                        Error (Unhandled (Parse, Unmatched (l', c', expr')), r''')
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
                }
                ))
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

