open Token;;

type lit = 
    | Nil
    | Bool   of bool
    | Number of float
    | String of string
    | Expr   of expr

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
    | Unhandled of (tokentype * int * int) (* token line col *)

[@@deriving show];;

let rec _expression tseq = 
    _equality tseq 

and _equality tseq' = 
    let (l_expr, tseq'') = _comp tseq' in 

    let check ts = 
        match Seq.uncons ts with
        | Some ((p, _l, _c), r) ->
            (match p with
                | BANG_EQUAL  -> 
                    let r_expr, _ts' = _comp r in
                    (Binary (l_expr, (Operator NotEq), r_expr), _ts')
                | EQUAL_EQUAL -> 
                    let r_expr, _ts' = _comp r in
                    (Binary (l_expr, (Operator Eq), r_expr), _ts')
                | _ -> 
                    (l_expr, ts)
            )
        | None ->
            (l_expr, ts)
    in check tseq'' 

and _comp compseq  = 
    let (l_expr, compseq') = _term compseq in

    let check ts = 
        match Seq.uncons ts with
        | Some ((p, _l, _c), r) ->
            (match p with
                | GREATER  -> 
                    let r_expr, _ts' = _term r in
                    (Binary (l_expr, (Compr Greater), r_expr), _ts')
                | GREATER_EQUAL -> 
                    let r_expr, _ts' = _term r in
                    (Binary (l_expr, (Compr GreaterEq), r_expr), _ts')
                | LESS -> 
                    let r_expr, _ts' = _term r in
                    (Binary (l_expr, (Compr Lesser), r_expr), _ts')
                | LESS_EQUAL -> 
                    let r_expr, _ts' = _term r in
                    (Binary (l_expr, (Compr Lesser), r_expr), _ts')
                | _ -> 
                    (l_expr, ts)
            )
        | None ->
            (l_expr, ts)
    in check compseq' 

and _term termseq =  
    let (l_expr, termseq') = _factor termseq in

    let check ts = 
        match Seq.uncons ts with
        | Some ((p, _l, _c), r) ->
            (match p with
                | GREATER  -> 
                    let r_expr, _ts' = _term r in
                    (Binary (l_expr, (Compr Greater), r_expr), _ts')
                | GREATER_EQUAL -> 
                    let r_expr, _ts' = _term r in
                    (Binary (l_expr, (Compr GreaterEq), r_expr), _ts')
                | LESS -> 
                    let r_expr, _ts' = _term r in
                    (Binary (l_expr, (Compr Lesser), r_expr), _ts')
                | LESS_EQUAL -> 
                    let r_expr, _ts' = _term r in
                    (Binary (l_expr, (Compr LesserEq), r_expr), _ts')
                | _ -> 
                    (l_expr, ts)
            )
        | None ->
            (l_expr, ts)
    in check termseq' 

and _factor facseq =
    let (l_expr, facseq') = _factor facseq in

    let check ts = 
        match Seq.uncons ts with
        | Some ((p, _l, _c), r) ->
            (match p with
                | SLASH  -> 
                    let r_expr, _ts' = _term r in
                    (Binary (l_expr, (Factor Div), r_expr), _ts')
                | STAR -> 
                    let r_expr, _ts' = _term r in
                    (Binary (l_expr, (Factor Mul), r_expr), _ts')
                | _ -> 
                    (l_expr, ts)
            )
        | None ->
            (l_expr, ts)
    in check facseq' 

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
            | LEFT_PAREN ->
                let expr', r' = _expression r in
                (match Seq.uncons r' with
                    | Some ((RIGHT_PAREN, _l, _c), r'') -> 
                        (Grouping expr', r'')
                    | Some ((p, l, c), _) -> 
                        failwith (Format.sprintf "Error - unmatched quote expected at line %d col %d! found: %s" l c (show_tokentype p))
                    | _ -> 
                        failwith (Format.sprintf "Error - unmatched quote line %d col %d! found: %s" l' c' (show_tokentype p))
                )
            | t -> 
                (Unhandled (t, l', c'), r)
        )
    | None ->
        (Literal Nil, pseq)
;;

