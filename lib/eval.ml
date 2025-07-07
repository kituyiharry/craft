open Ast;;
open Env;;
open Func;;

let (let*) = Result.bind;;

let rec eval (env) = function
    | Literal  l  -> (match l with
            | (VarIdent n) -> let* g = (Env.get env n) in 
                   Ok (g, env)
            | _ -> Ok (l, env)
        )
    | Assign (p, expr) -> 
        let* (g, env') = eval env expr in 
        let*     env'' = Env.assign env' p g in
        Ok (g, env'')
    | Grouping g -> eval env g
    | Unhandled (_t, s) -> Error s
    | Call (_args, _ident, _arity) -> 
        (* resolve all arguments in case they are expressions *)
        let* (_args', _env') = List.fold_left (fun call ex ->
            (match call with 
                | (Ok (args, env')) -> 
                    let* (g', env'') = eval env' ex in
                    Ok ((g' :: args), env'')
                | e -> e
            )
        ) (Ok ([], env)) _args in

        let _env''  = Env.spawn _env' in
        let* (l, e) = Func.call { func=_ident;arty=_arity;envr=_env'';argl=_args'; } in 

        Ok (l, Env.parent e)
    | Unary (op, u) ->
        let* (u', env') = eval env u in
        (match op with
            | Negate -> (match u' with
                    | Number f -> (Ok (Number (Float.neg f), env'))
                    | n -> Error (BadExp (u, Some n))
                )
            | Invert -> (match u' with
                    | Bool b -> Ok (Bool (Bool.not b), env')
                    | n -> Error (BadExp (u, Some n))
                )
        )
    | Binary (l, op, r) ->
        (match op with
            | Compr o -> 
                let* (l', env') = eval env  l in
                let* (r', env') = eval env' r in
                (match o with
                | Greater    -> (match (l', r') with
                    | (Number l'', Number r'') -> 
                        Ok (Bool ((Float.compare l'' r'') = 1), env')
                    | (x, y) ->
                        Error (TypeError (x, op, y))
                    )
                | GreaterEq -> (match (l', r') with
                    | (Number l'', Number r'') ->
                        let x = Float.compare l'' r'' in
                        Ok (Bool (x = 1 || x = 0), env')
                    | (x, y) ->
                        Error (TypeError (x, op, y))
                    )
                | Lesser -> (match (l', r') with
                    | (Number l'', Number r'') -> 
                        Ok (Bool ((Float.compare l'' r'') = -1), env')
                    | (x, y) ->
                        Error (TypeError (x, op, y))
                    )
                | LesserEq -> (match (l', r') with
                    | (Number l', Number r') ->
                        let x = Float.compare l' r' in
                        Ok (Bool (x = -1 || x = 0), env')
                    | (x, y) ->
                        Error (TypeError (x, op, y))
                    )
                )
            | Operator o -> 
                let* (l', env') = eval env  l in
                let* (r', env') = eval env' r in
                (match o with
                | Eq    -> (match ( l',  r') with
                    | (Bool   l'', Bool   r'') -> Ok (Bool ((=) l'' r''), env')
                    | (Number l'', Number r'') -> Ok (Bool (Float.equal l'' r''), env')
                    | (x, y) ->
                        Error (TypeError (x, op, y))
                    )
                | NotEq -> (match ( l',  r') with
                    | (Bool   l'', Bool   r'') -> Ok (Bool ((!=) l'' r''), env')
                    | (Number l'', Number r'') -> Ok (Bool (not @@ Float.equal l'' r''), env')
                    | (x, y) ->
                        Error (TypeError (x, op, y))
                    )
                )
            | Term     t -> 
                let* (l', env') = eval env  l in
                let* (r', env') = eval env' r in
                (match ( l',  r') with
                    | (Number l'', Number r'') -> (match t with
                        | Add -> Ok (Number (Float.add l'' r''), env')
                        | Sub -> Ok (Number (Float.sub l'' r''), env')
                        )
                    | (String l'', String r'') -> (match t with
                        | Add -> Ok (String (String.cat l'' r''), env')
                        | _ ->   Error (BadOp (l', op, r'))
                        )
                    | (x, y) ->
                        Error (TypeError (x, op, y))
                )
            | Factor   f -> 
                let* (l', env') = eval env  l in
                let* (r', env') = eval env' r in
                (match ( l',  r') with
                    | (Number l'', Number r'') -> (match f with
                        | Div -> Ok (Number (Float.div l'' r''), env')
                        | Mul -> Ok (Number (Float.mul l'' r''), env')
                        )
                    | (x, y) ->
                        Error (TypeError (x, op, y))
                )
            | Logic log -> 
                (*  Short Circuit *)
                let* (l', env') = eval env  l in
                (match (l') with 
                    | (Bool l'') -> 
                        (match log with
                            | And -> 
                                if l'' then
                                    let* (r', env') = eval env' r in
                                    Ok (r', env') 
                                else
                                    Ok (Bool (l''), env')
                            | Or -> 
                                if l'' then
                                    Ok (Bool (l''), env')
                                else
                                    let* (r', env') = eval env' r in
                                    Ok (r', env') 
                        )
                    | (x) ->
                        Error (BadCond (l, Some x))
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
                | Stmt (Ret _l)  ->
                    (* return statement ends execution of the current sequence of declarations *)
                    { prg=(Program { s with state=(_ast :: s.state) } ); env=env }
                | Stmt (Raw (Eval (Literal Eol)))  ->
                    foldast (s, env) more
                | Stmt (Raw (Eval e'))  -> (match (eval env e') with
                    |  Ok (o, env')    ->
                        foldast ({ s with state = ((mkraw o) :: s.state) }, env') more
                    |  Error err ->
                        (* TODO: pass line and col context too  *)
                        (* FIXME: tokens ?? *)
                        foldast ({ s with errs= (Unhandled (Eval, err) :: s.errs) }, env) more
                    )
                | Stmt ((Side (Effect (Println e')))) -> (match (eval env e') with
                    |  Ok (o, env')    -> let _ = (match o with
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
                        foldast ({ s with state = ((mkraw o) :: s.state) }, env') more
                    |  Error err ->
                        foldast ({ s with errs=(Unhandled (Eval, err) :: s.errs) }, env) more
                    )
                | Stmt ((Side (Effect (Print e')))) -> (match (eval env e') with
                    |  Ok (o, env')    -> let _ = (match o with
                            | (Bool b) ->
                                Format.printf "%b" b
                            | (Number n) ->
                                Format.printf "%f" n
                            | (String s) ->
                                Format.printf "%s" s
                            | Nil ->
                                Format.printf "nil"
                            |_ ->
                                (* TODO: runtime error *)
                                ()
                        ) in
                        foldast ({ s with state = ((mkraw o) :: s.state) }, env') more
                    |  Error err ->
                        foldast ({ s with errs=(Unhandled (Eval, err) :: s.errs) }, env) more
                    )
                | VarDecl (name, exp) ->
                    (match eval env exp with
                    | Ok (o, env') ->
                            foldast (
                                { s with state = ((mkraw o) :: s.state) }, 
                                (Env.define name o env')
                            ) more
                    | Error err -> 
                        foldast ({ s with errs = ((Unhandled (Eval, err)) :: s.errs) }, env) more)
                | Block (stmts) ->
                    let env' = Env.spawn env in 
                    let {env;prg=(Program t)} = foldast ({ state=[]; errs=[] }, env') (List.to_seq stmts) in
                    let oenv = Env.parent env in
                    foldast (({ state=(s.state @ t.state); errs=(s.errs @ t.errs) }), oenv) more
                | Branch (If (exp, ifblck, elseblk)) ->
                    (match eval env exp with
                        | Ok ((Bool b), env) -> 
                            if b then
                                let env' = Env.spawn env in 
                                let {env;prg=(Program t)} = foldast ({ state=[]; errs=[] }, env') (Seq.return ifblck) in
                                let oenv = Env.parent env in
                                foldast (({ state=(s.state @ t.state); errs=(s.errs @ t.errs) }), oenv) more
                            else
                                (match elseblk with
                                | Some els ->
                                    let env' = Env.spawn env in 
                                    let {env;prg=(Program t)} = foldast ({ state=[]; errs=[] }, env') (Seq.return els) in
                                    let oenv = Env.parent env in
                                    foldast (({ state=(s.state @ t.state); errs=(s.errs @ t.errs) }), oenv) more 
                                | _ ->
                                    foldast (s, env) more
                                )
                        | Ok (l, env) ->
                            let err = Unhandled (Eval, BadCond(exp, Some l))  in
                            foldast ({ s  with errs = (err :: s.errs) }, env) more
                        | Error err ->
                            let err = Unhandled (Eval, err)  in
                            foldast ({ s  with errs = (err :: s.errs) }, env) more
                    )
                | Loop (While (exp, loopblk)) ->
                    let rec loop (s, env) =
                        (match eval env exp with
                            | Ok ((Bool b), env') -> 
                                (* can be optimized *)
                                if b then
                                    let env'' = Env.spawn env' in 
                                    let {env;prg=(Program t)} = foldast ({ state=[]; errs=[] }, env'') (Seq.return loopblk) in
                                    let oenv = Env.parent env in
                                    loop (({ state=(s.state @ t.state); errs=(s.errs @ t.errs) }), oenv)
                                else
                                    foldast (s, env') more
                            | Ok (l, env) ->
                                let err = Unhandled (Eval, BadCond(exp, Some l))  in
                                foldast ({ s  with errs = (err :: s.errs) }, env) more
                            | Error err ->
                                let err = Unhandled (Eval, err)  in
                                foldast ({ s  with errs = (err :: s.errs) }, env) more
                        )
                    in loop (s, env)

                | FunDecl (name, args, arity, block) -> 

                    let impl = Native.impl name (foldast) args block in
                    let env = Env.define name (FunImpl (arity, impl)) env in
                    foldast (s, env) more

                | Return exp ->
                        (match eval env exp with 
                            | Ok (l, env') -> 
                                { prg=Program ({ s with state=((Stmt (Ret l)) :: s.state) }); env=env' }
                            | Error e ->
                                { prg=Program ({ s with errs=((Unhandled (Eval, e)) :: s.errs) }); env=env }
                        )

                | Loop (For (init, _condn, _assgn, _blck)) ->

                    let env' = Env.spawn env in
                    let blk  = Seq.return _blck in

                    let rec check (s, env) = 
                        match eval env _condn with
                        | Ok ((Bool b), env') -> 
                            if b then
                                let {env=env';prg=(Program s')} = foldast (s, env) blk in
                                (match eval env' _assgn with
                                    | Ok (p, env'') -> 
                                        check ({ s' with state=((mkraw p) :: s'.state) }, env'')
                                    | Error err -> 
                                        let err = Unhandled (Eval, err)  in
                                        ({ s' with errs=(err :: s'.errs) }, env')
                                )
                            else
                                (s, env')
                        | Ok (l, env') ->
                            let err = Unhandled (Eval, BadCond (_condn, Some l)) in
                            ({ s with errs = (err :: s.errs) }, env')
                        | Error err -> 
                            let err = Unhandled (Eval, err)  in
                            ({ s with errs = (err :: s.errs) }, env')
                    in 

                    match init with
                    | LoopDecl (name, exp) ->
                        (match eval env' exp with
                            | Ok (o, env') ->

                                let s'   = { s with state = ((mkraw o) :: s.state) } in 
                                let env' = Env.define name o env' in
                                let (s'', env'') = check (s', env') in

                                foldast ({ state=(s'.state @ s''.state); errs=(s''.errs @ s'.errs) }, Env.parent env'')  more

                            | Error err -> 
                                foldast ({ s with errs = ((Unhandled (Eval, err)) :: s.errs) }, env) more
                        )
                    | LoopStmt _st ->
                        match eval env' _st with
                        | Ok (o, env') -> 
                            let (s'', env'') = check ({ s with state = ((mkraw o) :: s.state) }, env') in
                            foldast ({ state=(s.state @ s''.state); errs=(s''.errs @ s.errs) }, Env.parent env'')  more
                        | Error err -> 
                            foldast ({ s with errs = ((Unhandled (Eval, err)) :: s.errs) }, env) more
                )
            | _ ->
                { prg=Program (s); env=env }
        )
    in 
    let env = Env.define "clock" (FunImpl (0, Native.clock)) Env.empty in
    foldast ({ state=[]; errs=errs }, env) astseq
;;
