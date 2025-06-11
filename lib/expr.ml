(*
    Recursive descent parser
*)
open Effect;;
open Effect.Deep;;
open Token;;

type tokseq = (tokentype * int * int ) Seq.t 

type _ Effect.t += 
    | Synchronize: (tokseq * string) -> tokseq Effect.t
;;

type lit = 
    | Nil
    | Close
    | Bool   of bool
    | Number of float
    | String of string

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
    | Literal  of lit
    | Factor   of factor
    | Term     of term
    | Compr    of comparison
    | Operator of equality
    | Unary    of (unary * expr)
    | Binary   of (expr * expr * expr)
    | Grouping of expr 
    | Stmts    of expr list
    | Unhandled of (tokentype * int * int) (* token line col *)

[@@deriving show];;

let rec _expression tseq = 

    let (ast', ts') = _equality tseq in 
    (ast', ts')

and _equality tseq' = 
    let (lexpr, tseq'') = _comp tseq' in 

    let rec check l_expr ts = 
        match Seq.uncons ts with
        | Some ((p, _l, _c), r) ->
            (match p with
                | BANG_EQUAL  -> 
                    let r_expr, _ts' = _comp r in
                    let l_expr' = (Binary (l_expr, (Operator NotEq), r_expr)) 
                    in check l_expr' _ts' 
                | EQUAL_EQUAL -> 
                    let r_expr, _ts' = _comp r in
                    let l_expr' = (Binary (l_expr, (Operator Eq), r_expr)) 
                    in check l_expr' _ts' 
                | _ -> 
                    (l_expr, ts)
            )
        | None ->
            (l_expr, ts)
    in check lexpr tseq'' 

and _comp compseq  = 
    let (lexpr, compseq') = _term compseq in

    let rec check l_expr ts = 
        match Seq.uncons ts with
        | Some ((p, _l, _c), r) ->
            (match p with
                | GREATER  -> 
                    let r_expr, _ts' = _term r in
                    let lexpr' = (Binary (l_expr, (Compr Greater), r_expr)) in 
                    check lexpr' _ts'
                | GREATER_EQUAL -> 
                    let r_expr, _ts' = _term r in
                    let lexpr' = (Binary (l_expr, (Compr GreaterEq), r_expr)) in
                    check lexpr' _ts'
                | LESS -> 
                    let r_expr, _ts' = _term r in
                    let lexpr' = (Binary (l_expr, (Compr Lesser), r_expr)) in 
                    check lexpr' _ts'
                | LESS_EQUAL -> 
                    let r_expr, _ts' = _term r in
                    let lexpr' = (Binary (l_expr, (Compr Lesser), r_expr)) in
                    check lexpr' _ts'
                | _ -> 
                    (l_expr, ts)
            )
        | None ->
            (l_expr, ts)
    in check lexpr compseq' 

and _term termseq =  
    let (lexpr, termseq') = _factor termseq in

    let rec check l_expr ts = 
        match Seq.uncons ts with
        | Some ((p, _l, _c), r) ->
            (match p with
                | MINUS  -> 
                    let r_expr, _ts' = _term r in
                    let lexpr' = (Binary (l_expr, (Term Sub), r_expr)) in
                    check lexpr' _ts'
                | PLUS -> 
                    let r_expr, _ts' = _term r in
                    let lexpr' = (Binary (l_expr, (Term Add), r_expr)) in
                    check lexpr' _ts'
                | _ -> 
                    (l_expr, ts)
            )
        | None ->
            (l_expr, ts)
    in check lexpr termseq' 

and _factor facseq =
    let (lexpr, facseq') = _unary facseq in

    let rec check l_expr ts = 
        match Seq.uncons ts with
        | Some ((p, _l, _c), r) ->
            (match p with
                | SLASH  -> 
                    let r_expr, _ts' = _term r in
                    let lexpr' = (Binary (l_expr, (Factor Div), r_expr)) in
                    check lexpr' _ts'
                | STAR -> 
                    let r_expr, _ts' = _term r in
                    let lexpr' = (Binary (l_expr, (Factor Mul), r_expr)) in 
                    check lexpr' _ts'
                | _ -> 
                    (l_expr, ts)
            )
        | None ->
            (l_expr, ts)
    in check lexpr facseq' 

and _unary useq = 

    (match Seq.uncons useq with
    | Some ((p, _l, _c), r) ->
        (match p with
            | BANG  -> 
                let r_expr, _ts' = _unary r in
                (Unary (Invert, r_expr), _ts')
            | MINUS -> 
                let r_expr, _ts' = _unary r in
                (Unary (Negate, r_expr), _ts')
            | _ -> 
                primary useq
        )
    | None ->
        (Literal Nil, useq)
    ) 

and primary pseq = 

    match Seq.uncons pseq with
    | Some ((p, l', c'), r) ->
        (match p with
            | FALSE      -> (Literal (Bool false), r)
            | TRUE       -> (Literal (Bool true) , r)
            | NUMBER f   -> (Literal (Number f)  , r)
            | STRING s   -> (Literal (String s)  , r)
            | SEMICOLON  -> (Literal Close, r) (* terminate *)
            | LEFT_PAREN ->
                let expr', r' = _expression r in
                (match Seq.uncons r' with
                    | Some ((RIGHT_PAREN, _l, _c), r'') -> 
                        (Grouping expr', r'')
                    | Some ((p, l, c), r'') -> 
                        let err = (Format.sprintf "Error - unmatched parentheses expected at line %d col %d! found: %s" l c (show_tokentype p)) in
                        let r''' = perform (Synchronize (r'', err)) in 
                        (Unhandled (p, l, c), r''')
                    | _ -> 
                        let err = (Format.sprintf "Error - unmatched parentheses line %d col %d! found: %s" l' c' (show_tokentype p)) in
                        let r''' = perform (Synchronize (r', err)) in 
                        (Unhandled (p, l', c'), r''')
                )
            | t -> 
                (Unhandled (t, l', c'), r)
        )
    | None ->
        (Literal Nil, pseq)
;;

 let parse tseq = 
    let rec _parse stmts ts =
        try 
            let (stmt, more) = _expression ts in 
            if Seq.is_empty more then
                Stmts (List.rev (stmt :: stmts))
            else
                _parse (stmt :: stmts) more
        with 
            (* failover for parse errors *)
            | effect Synchronize (t, e), k -> 
                let _ = Format.printf "Synchronize from Error: %s\n" e in
                let t'= (Seq.drop_while (fun (p, _l, _c) -> 
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
                ) t) in
                match Seq.uncons t' with
                | Some ((SEMICOLON, _, _), r) ->
                        continue k r
                | _ -> 
                    continue k t'
    in _parse [] tseq
;;

