open Ast;;
open Env;;

let (let*) = Result.bind;;

type craftenv = {
        prg: source 
    ;   env: Env.t
}

let rec eval (env) = function
    | Literal  l  -> (match l with
            | (VarIdent n) -> let* g = (Env.get env n) in Ok g
            | _ -> Ok l
        )
    | Grouping g  -> eval env g
    | Unhandled (_t, s) -> Error s
    | Unary (op, u) ->
        let* u' = eval env u in
        (match op with
            | Negate -> (match u' with
                    | Number f -> (Ok (Number (Float.neg f)))
                    | n -> Error (BadExp (u, Some n))
                )
            | Invert -> (match u' with
                    | Bool b -> Ok (Bool (Bool.not b))
                    | n -> Error (BadExp (u, Some n))
                )
        )
    | Binary   (l, op, r) ->
        let* l' = eval env l in
        let* r' = eval env r in
        (match op with
            | Compr o -> (match o with
                | Greater    -> (match (l', r') with
                    | (Number l'', Number r'') -> Ok (Bool ((Float.compare l'' r'') = 1))
                    | (x, y) ->
                        Error (BadMatch (x, op, y))
                    )
                | GreaterEq -> (match (l', r') with
                    | (Number l'', Number r'') ->
                        let x = Float.compare l'' r'' in
                        Ok (Bool (x = 1 || x = 0))
                    | (x, y) ->
                        Error (BadMatch (x, op, y))
                    )
                | Lesser -> (match (l', r') with
                    | (Number l'', Number r'') -> Ok (Bool ((Float.compare l'' r'') = -1))
                    | (x, y) ->
                        Error (BadMatch (x, op, y))
                    )
                | LesserEq -> (match (l', r') with
                    | (Number l', Number r') ->
                        let x = Float.compare l' r' in
                        Ok (Bool (x = -1 || x = 0))
                    | (x, y) ->
                        Error (BadMatch (x, op, y))
                    )
                )
            | Operator o -> (match o with
                | Eq    -> (match ( l',  r') with
                    | (Bool   l'', Bool   r'') -> Ok (Bool ((=) l'' r''))
                    | (Number l'', Number r'') -> Ok (Bool (Float.equal l'' r''))
                    | (x, y) ->
                        Error (BadMatch (x, op, y))
                    )
                | NotEq -> (match ( l',  r') with
                    | (Bool   l'', Bool   r'') -> Ok (Bool ((!=) l'' r''))
                    | (Number l'', Number r'') -> Ok (Bool (not @@ Float.equal l'' r''))
                    | (x, y) ->
                        Error (BadMatch (x, op, y))
                    )
                )
            | Term     t -> (match ( l',  r') with
                    | (Number l'', Number r'') -> (match t with
                        | Add -> Ok (Number (Float.add l'' r''))
                        | Sub -> Ok (Number (Float.sub l'' r''))
                        )
                    | (String l'', String r'') -> (match t with
                        | Add -> Ok (String (String.cat l'' r''))
                        | _ ->   Error (BadOp (l', op, r'))
                        )
                    | (x, y) ->
                        Error (BadMatch (x, op, y))
                )
            | Factor   f -> (match ( l',  r') with
                    | (Number _, Number (0.)) ->
                        Error (BadOp (l', op, r'))
                    | (Number l'', Number r'') -> (match f with
                        | Div -> Ok (Number (Float.div l'' r''))
                        | Mul -> Ok (Number (Float.mul l'' r''))
                        )
                    | (x, y) ->
                        Error (BadMatch (x, op, y))
                )
            | n -> Error (BadExp (n, None))
        )
    | e -> Error (BadExp (e, None))
;;

let mkraw l =
    Stmt (Raw (Eval (Literal l)))
;;

let eval_exprs (Program {state=el;errs}) =
    let astseq = (List.to_seq el) in

    let rec foldast (s, env) tseq =
        (match Seq.uncons tseq with
            | Some ((_ast), more) ->
                (match _ast with
                (* newline *)
                | Stmt (Raw (Eval (Literal Eol)))  ->
                    foldast (s, env) more
                | Stmt (Raw (Eval e'))  -> (match (eval env e') with
                    |  Ok o    ->
                        foldast ({ s with state = ((mkraw o) :: s.state) }, env) more
                    |  Error err ->
                        (* TODO: pass line and col context too  *)
                        (* FIXME: tokens ?? *)
                        foldast ({ s with errs= (Unhandled (Eval, err) ::
                        s.errs) }, env) more
                    )
                | Stmt ((Side (Effect (Print e')))) -> (match (eval env e') with
                    |  Ok o    -> let _ = (match o with
                            | (Bool b) ->
                                Format.printf "%b\n" b
                            | (Number n) ->
                                Format.printf "%f\n" n
                            | (String s) ->
                                Format.printf "%s\n" s
                            | Nil ->
                                Format.printf "nil\n"
                            |_ ->
                                (* TODO: runtime error *)
                                ()
                        ) in
                        foldast ({ s with state = ((mkraw o) :: s.state) }, env) more
                    |  Error err ->
                        foldast ({ s with errs=(Unhandled (Eval, err) :: s.errs) }, env) more
                    )
                | VarDecl (name, exp) ->
                    (* TODO: handle *)
                    match eval env exp with
                    | Ok o ->
                            foldast (
                                { s with state = ((mkraw o) :: s.state) }, 
                                (Env.define name o env)
                            ) more
                    | Error err -> 
                        foldast ({ s with errs = ((Unhandled (Eval, FailedEval err)) :: s.errs) }, env) more
                )
            | _ ->
                { prg=Program (s); env=env }
        )
    in foldast ({ state=[]; errs=errs }, Env.empty) astseq
;;
