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

(* scope resolution helpers *)
let rec expresolve res exp = 
    (match exp with
        | Literal (_lit) -> 
            (match _lit with
                | VarIdent name -> 
                    (match res with
                        | { Resolver.locals } :: _rem -> 
                            (match Resolver.ScopeMap.find_opt name locals  with 
                                | Some false ->
                                    Error ("Initializer reuse of " ^ name)
                                |  _ -> Ok res
                            )
                        | [] -> 
                            Ok res
                    )
                | _ -> 
                    Ok res
            ) 
        | Factor    (_factor) -> Ok res
        | Term      (_term) -> Ok res
        | Compr     (_comparison) -> Ok res
        | Operator  (_equality) -> Ok res
        | Unary     (_unary, _expr) -> Ok res
        | Binary    (_lexp,  _op, _rexp) -> Ok res
        | Grouping  (_expr) -> Ok res 
        | Logic     (_logical) -> Ok res
        | Assign    (_string, _expr) -> Ok res
        | Unhandled (_stage,  _crafterr) -> Ok res 
        | Call      (_callargs, _funcname, _callarity) -> Ok res
    )

and dclresolve res dcl = 
    (match dcl with
        | VarDecl (_name, _exp) -> 
            let res' = Resolver.declare res _name in
            (match _exp with
                | Literal Nil -> 
                    Ok (Resolver.define (res') _name)
                | _ -> 
                    let* res' = expresolve res' _exp in
                    Ok (Resolver.define res' _name)
            )
        | Stmt  (_stmt) -> Ok res
        | Block (blcks) -> 
            let res'  = Resolver.begin_scope res in 
            let* res' = List.fold_left (fun acc dcl ->
                (match acc with 
                    | Ok res -> dclresolve res dcl 
                    | e -> e
                )
            ) (Ok res') blcks in
            let res  = Resolver.end_scope res' in
            Ok res
        | Branch  (_branch) -> Ok res
        | Loop    (_loop)   -> Ok res
        | FunDecl (_name, _args, _arity, _blck) -> Ok res
        | Return  (_exp)    -> Ok res 
    )

and prgresolve res { state; _ } =
    List.fold_left (fun acc dcl -> 
        match acc with 
        | Ok r -> dclresolve r dcl 
        | e    -> e 
    ) (Ok res) state
;;

let rec _program  tseq = 

    match Seq.uncons tseq with
    | Some(((PRINT), _, _), tseq') -> 
        (* print "stuff" *)
        _printstmt  tseq'
    | Some(((PRINTLN), _, _), tseq') -> 
        (* printl "stuff with newline" *)
        _printlnstmt  tseq'
    | Some(((VAR), _, _), tseq') -> 
        (* var x = 100; *)
        _vardecl  tseq'
    | Some (((LEFT_BRACE), l, c), tseq') -> 
        (* { /* more stuff in braces */ } *)
        _blockstmts  (l, c) [] tseq'
    | Some (((IF), l, c), tseq') -> 
        (* if (cond) { //more stuff in optional braces } *)
        _ifstmts  (l, c) tseq'
    | Some (((WHILE), l, c), tseq') -> 
        (* while (cond) { /* do stuff */ } *)
        _whilestmt  (l, c) tseq'
    | Some (((FOR), l, c), tseq') -> 
        (* for (var i = 0; i < max; i = i + 1) { /* stuff */ } *)
        _forstmt  (l, c) tseq'
    | Some ((FUN, l, c), fncseq') ->
        (* fun name (...) { /* do stuff */ } *)
        _funcblock  (l, c) fncseq'
    | Some ((IDENTIFIER _ident, _, _), _) ->
        (* x = something; *)
        _assign  tseq
    | Some ((RETURN, _, _), rseq') ->
        (* return something; *)
        _retstmt  rseq'
    | _ -> 
        (* 1 + 1 - (2 * 3 / 4) *)
        _express  tseq

and _retstmt  rseq = 
    let* (exp, rem) = _expression  rseq in
    Ok ((Return exp), rem)

and _funcblock  (l, c) fncseq = 

    match Seq.uncons fncseq with
    | Some ((IDENTIFIER name, l, c), more) -> 
        (match Seq.uncons more with
        | Some ((LEFT_PAREN, l, c), more') -> 
            let* exp, rem = _callexpr  (l, c) name more' in
            (match exp with
                | Call (exl, name, arty) -> 
                    let* exl' = List.fold_left (fun acc ac -> 
                        (match acc with 
                            | Ok r ->  
                                (match ac with 
                                    | Literal (VarIdent _ as v) -> (Ok (v :: r))
                                    | e ->
                                        Error (Unhandled (Parse, BadExp (e, None)), rem)
                                 )
                            | e    -> e
                        )
                    ) (Ok []) exl in
                    let* (blck, left) = _program  rem in
                    Ok (FunDecl (name, exl', arty, blck), left)
                | e -> 
                    (* likely unreachable *)
                    Error (Unhandled (Parse, (BadExp (e, None))), fncseq)
            )
        | _ ->
            Error (Unhandled (Parse, (Unexpected (l, c, LEFT_PAREN))), fncseq)
        )
    | _ -> 
        Error (Unhandled (Parse, (Unexpected (l, c, LEFT_PAREN))), fncseq)

and _forstmt  (l, c) fseq =

    (* NB: can also be desugared into a while loop  *)
    match Seq.uncons fseq with
    | Some ((LEFT_PAREN, l, c), more) -> 
        let* (exp, more') = _program  more in
        (match exp with
            | VarDecl dcl -> 
                (match Seq.uncons more' with
                    | Some ((SEMICOLON, l, c), rem) -> 
                        let* (exp, rem') = _expression  rem in
                        (match Seq.uncons rem' with 
                            | Some ((SEMICOLON, _l, _c), rem'') -> 
                                let* (asg, rem''') = _assignment  rem'' in
                                (match Seq.uncons rem''' with
                                    | Some ((RIGHT_PAREN, _l, _c), fin) -> 
                                        let* (loopblk, fin) = _program  fin in
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
                        let* (exp, rem') = _expression  rem in
                        (match Seq.uncons rem' with 
                            | Some ((SEMICOLON, _l, _c), rem'') -> 
                                let* (asg, rem''') = _assignment  rem'' in
                                (match Seq.uncons rem''' with
                                    | Some ((RIGHT_PAREN, _l, _c), fin) -> 
                                        let* (loopblk, fin) = _program  fin in
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

and _whilestmt  (l, c) ifseq = 

    match Seq.uncons ifseq with
    | Some ((LEFT_PAREN, l, c), more) -> 
        let* (exp, more') = _expression  more in
        (match Seq.uncons more' with
        | Some ((RIGHT_PAREN, _, _), left) -> 
            let* (loopbr, left') = _program  left in 
            Ok (Loop (While (exp, loopbr)), left')
        | _ ->
            Error (Unhandled (Parse, (Unmatched (l, c, RIGHT_PAREN))), ifseq)
        )
    | Some ((_, l,c), _) -> 
        Error (Unhandled (Parse, (Unexpected (l, c, LEFT_PAREN))), ifseq)
    | _ -> 
        Error (Unhandled (Parse, (Unexpected (l, c, LEFT_PAREN))), ifseq)

and _ifstmts  (l, c) ifseq = 

    match Seq.uncons ifseq with
    | Some ((LEFT_PAREN, l, c), more) -> 
        let* (exp, more') = _expression  more in
        (match Seq.uncons more' with
        | Some ((RIGHT_PAREN, _, _), left) -> 
            let* (thenbr, left') = _program  left in 
            (match Seq.uncons left' with
                | Some ((ELSE, _, _), more) -> 
                    let* (elsebr, left'') = _program  more in 
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

and _blockstmts  (l, c) stmts bseq =
    let rec check  (l, c) stmts bseq =
        match Seq.uncons bseq with
        | Some ((RIGHT_BRACE, _, _), more) -> 
            let blck  = Block (List.rev stmts) in
            Ok (blck, more)
        | Some ((SEMICOLON, l, c), more) -> 
            check  (l, c) stmts more
        | Some ((_, _, _), _) ->
            let* (exp, more') = _program bseq in 
            (match Seq.uncons more' with
                | Some ((_, l, c), _) -> 
                    check  (l, c) (exp :: stmts) more'
                | _ -> 
                    check  (l, c) (exp :: stmts) more'
            )
        | _ -> 
            Error (Unhandled (Parse, (Unmatched (l, c, RIGHT_BRACE))), bseq)
    in check  (l, c) stmts bseq

and _assign  aseq = 
    let* (ast', ts') = _assignment  aseq in
    Ok ((Stmt (Raw (Eval ast'))), ts')

and _assignment  aseq = 
    let* (exp, rem) = _expression  aseq in

    (match Seq.uncons rem with
        | Some(((EQUAL), _, _), aseq') -> 
            let* (exp', rem') = _assignment  aseq' in 
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

and _express  eseq = 
    let* (ast', ts') = _expression  eseq in 
    Ok ((Stmt (Raw (Eval ast'))), ts')

and _printstmt  pseq =
    let* (ast', ts') = _expression  pseq in 
    Ok (Stmt (Side (Effect (Print ast'))), ts')

and _printlnstmt  pseq =
    let* (ast', ts') = _expression  pseq in 
    Ok (Stmt (Side (Effect (Println ast'))), ts')

and _vardecl vseq = 

    match Seq.uncons vseq with
    | Some(((IDENTIFIER ident), _, _), tseq') -> 
        (match Seq.uncons tseq' with
            | Some(((EQUAL), _, _), tseq'') -> 
                let* (ast, ts) = _expression  tseq'' in 
                Ok (VarDecl (ident, ast), ts)
            | Some ((SEMICOLON, _l, _c), tseq'') -> 
                Ok (VarDecl (ident, (Literal Nil)), tseq'')
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

and _expression  exseq' = 
    _logical  exseq'

and _logical  lseq' =
    _logicalor  lseq'

and _logicalor  oseq' =

    let* (lexpr, tseq'') = _logicaland  oseq' in 

    let rec check  l_expr ts = 
        match Seq.uncons ts with
        | Some ((p, _l, _c), r) ->
            (match p with
                | OR  -> 
                    let* r_expr, _ts' = _logicaland  r in
                    let l_expr' = (Binary (l_expr, (Logic Or), r_expr)) 
                    in check  l_expr' _ts' 
                | _ -> 
                    Ok (l_expr, ts)
            )
        | None ->
            Ok (l_expr, ts)
    in check  lexpr tseq'' 


and _logicaland  aseq' =

    let* (lexpr, tseq'') = _equality  aseq' in 

    let rec check  l_expr ts = 
        match Seq.uncons ts with
        | Some ((p, _l, _c), r) ->
            (match p with
                | AND  -> 
                    let* r_expr, _ts' = _equality  r in
                    let l_expr' = (Binary (l_expr, (Logic And), r_expr)) 
                    in check  l_expr' _ts' 
                | _ -> 
                    Ok (l_expr, ts)
            )
        | None ->
            Ok (l_expr, ts)

    in check  lexpr tseq'' 

and _equality  tseq' = 
    let* (lexpr, tseq'') = _comp  tseq' in 

    let rec check  l_expr ts = 
        match Seq.uncons ts with
        | Some ((p, _l, _c), r) ->
            (match p with
                | BANG_EQUAL  -> 
                    let* r_expr, _ts' = _comp  r in
                    let l_expr' = (Binary (l_expr, (Operator NotEq), r_expr)) 
                    in check  l_expr' _ts' 
                | EQUAL_EQUAL -> 
                    let* r_expr, _ts' = _comp  r in
                    let l_expr' = (Binary (l_expr, (Operator Eq), r_expr)) 
                    in check  l_expr' _ts' 
                | _ -> 
                    Ok (l_expr, ts)
            )
        | None ->
            Ok (l_expr, ts)
    in check  lexpr tseq'' 

and _comp  compseq  = 
    let* (lexpr, compseq') = _term  compseq in

    let rec check  l_expr ts = 
        match Seq.uncons ts with
        | Some ((p, _l, _c), r) ->
            (match p with
                | GREATER  -> 
                    let* r_expr, _ts' = _term  r in
                    let lexpr' = (Binary (l_expr, (Compr Greater), r_expr)) in 
                    check  lexpr' _ts'
                | GREATER_EQUAL -> 
                    let* r_expr, _ts' = _term  r in
                    let lexpr' = (Binary (l_expr, (Compr GreaterEq), r_expr)) in
                    check  lexpr' _ts'
                | LESS -> 
                    let* r_expr, _ts' = _term  r in
                    let lexpr' = (Binary (l_expr, (Compr Lesser), r_expr)) in 
                    check  lexpr' _ts'
                | LESS_EQUAL -> 
                    let* r_expr, _ts' = _term  r in
                    let lexpr' = (Binary (l_expr, (Compr LesserEq), r_expr)) in
                    check  lexpr' _ts'
                | _ -> 
                    Ok (l_expr, ts)
            )
        | None ->
            Ok (l_expr, ts)
    in check  lexpr compseq' 

and _term  termseq =  
    let* (lexpr, termseq') = _factor  termseq in

    let rec check  l_expr ts = 
        match Seq.uncons ts with
        | Some ((p, _l, _c), r) ->
            (match p with
                | MINUS  -> 
                    let* r_expr, _ts' = _term  r in
                    let lexpr' = (Binary (l_expr, (Term Sub), r_expr)) in
                    check  lexpr' _ts'
                | PLUS -> 
                    let* r_expr, _ts' = _term  r in
                    let lexpr' = (Binary (l_expr, (Term Add), r_expr)) in
                    check  lexpr' _ts'
                | _ -> 
                    Ok (l_expr, ts)
            )
        | None ->
            Ok (l_expr, ts)
    in check  lexpr termseq' 

and _factor  facseq =
    let* (lexpr, facseq') = _unary  facseq in

    let rec check  l_expr ts = 
        match Seq.uncons ts with
        | Some ((p, _l, _c), r) ->
            (match p with
                | SLASH  -> 
                    let* r_expr, _ts' = _term  r in
                    let lexpr' = (Binary (l_expr, (Factor Div), r_expr)) in
                    check  lexpr' _ts'
                | STAR -> 
                    let* r_expr, _ts' = _term  r in
                    let lexpr' = (Binary (l_expr, (Factor Mul), r_expr)) in 
                    check  lexpr' _ts'
                | _ -> 
                    Ok (l_expr, ts)
            )
        | None ->
            Ok (l_expr, ts)
    in check  lexpr facseq' 

and _unary  useq = 

    (match Seq.uncons useq with
    | Some ((p, _l, _c), r) ->
        (match p with
            | BANG  -> 
                let* r_expr, _ts' = _unary  r in
                Ok (Unary (Invert, r_expr), _ts')
            | MINUS -> 
                let* r_expr, _ts' = _unary  r in
                Ok (Unary (Negate, r_expr), _ts')
            | _ -> 
                call  useq
        )
    | None ->
        Ok (Literal Eol, useq)
    ) 

and call  cseq =  
    let* (ex, cseq') = primary  cseq in

    (match Seq.uncons cseq' with
        | Some ((LEFT_PAREN, _l, _c), r) ->
            (match ex with
            | Literal VarIdent name ->  
                _callexpr  (_l, _c) name r
            | _ -> 
                Error (Unhandled (Parse, UnCallable ex), r)
            )
        | _ ->  
            Ok (ex, cseq')
    )


and _callexpr  (l, c) ex ce = 
    let args = [] in
    let size = 0 in
    let rec check  size args cs = 
        if size >= _MaxArgs then 
            Error (Unhandled (Parse, MaxArgs (l, c, ex)), cs)
        else
        (match Seq.uncons cs with
            | Some ((RIGHT_PAREN, _l, _c), r) ->
                Ok (Call (args, ex, size), r)
            | _ -> 
                let* (ex', rem) = _expression  cs in
                (match Seq.uncons rem with
                    | Some ((COMMA, _l, _c), r) ->
                        check  (size + 1) (ex' :: args) r
                    | Some ((RIGHT_PAREN, _l, _c), r) ->
                        Ok (Call (ex' :: args, ex, (size + 1)),  r)
                    | _ -> 
                        Ok (Call (ex' :: args, ex, (size + 1)), rem)
                ) 
        )
    in check  size args ce

and primary  pseq = 

    match Seq.uncons pseq with
    | Some ((p, l', c'), r) ->
        (match p with
            | FALSE        -> Ok (Literal (Bool false), r)
            | TRUE         -> Ok (Literal (Bool true) , r)
            | NUMBER f     -> Ok (Literal (Number f)  , r)
            | STRING s     -> Ok (Literal (String s)  , r)
            | IDENTIFIER i -> Ok (Literal (VarIdent i), r)
            | LEFT_PAREN ->
                let* expr', r' = _expression  r in
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
    let res = Resolver.empty in
    let rec _parse s ts =
        match  _program ts with
        | Ok (stmt, more) ->
            if Seq.is_empty more then
                Ok (Program ({ s with
                    state = (List.rev (stmt :: s.state)); 
                    resl  = s.resl;
                }))
            else
                (match Seq.uncons more with
                    | Some ((SEMICOLON, _l', _c'), more') -> 
                        _parse { s with state=(stmt :: s.state); } more'
                    | _ ->
                        _parse { s with state=(stmt :: s.state) } more)
        | Error (e, more) -> 
            _parse { s with errs=(e :: s.errs); resl=res } more
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

