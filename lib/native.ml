open Resolver;;
open Ast;;
open Env;;

let clock _env _args = 
    Ok (Number (Unix.gettimeofday ()), _env) 
;;

let env _env _args = 
    let _ = Format.printf "----------------------\n %s \n----------------------\n" (Env.show _env) in
    Ok (Nil, _env) 
;;

let memo name (memargs: expr list) (_tbl: lit MemoTbl.t) (resl: lookup) (_interp: ((context * Ast.craftenv) -> decl Seq.t -> craftsrc)) (_args: lit list) (block) = 
    (*fun  env and expression *)
    (fun (_env: craftenv) (_args': lit list) -> 
        (* resolve arguments with their local names (map params to vars) *)
        let e' = (
            _args'
            |> List.to_seq
            |> Seq.zip (List.to_seq _args) 
            |> Seq.fold_left (fun e' (argn, argv) -> 
                match (argn, argv) with
                | (VarIdent n, v) -> Env.define n v e'
                | _ -> e'
            ) _env
        ) in 

        (* resolve memoized_args *)
        let sb = Buffer.create 40 in 
        let _ = Buffer.add_string sb name in

        let e' = Env.spawn e' in
        let _ = List.iter (fun n -> 
            match n with 
            | Literal (VarIdent n) -> 
                (match (Env.fetch e' resl n) with
                    | Ok v -> 
                        Buffer.add_string sb (show_lit v)
                    | Error e ->  
                        let _ = Format.printf "failed memo fetch: %s\n" (show_crafterr e) in
                        ()
                )
            | _ -> 
                (* expressions are ignored *)
                ()
        ) memargs in

        let cachekey = (Buffer.contents sb) in
        (match MemoTbl.find_opt _tbl cachekey with 
            | Some l -> 
                let e' = Env.parent e' in 
                Ok (l, e')
            | _ -> 
                (* run the interpreter! *)
                let { prg=(Program({ errs; state; _ })); env } = _interp ({ state=[]; errs=[]; resl=resl }, e') (Seq.return block) in
                let env = Env.parent env in 

                match (state, errs) with
                | (((Stmt (Ret l)) :: _rest), []) -> 
                    let _ = MemoTbl.add _tbl cachekey l in
                    Ok (l, env)
                | (_, []) -> 
                    Ok ((Nil), env)
                | (_, (_e :: _)) -> 
                    Error (ErrGroup (name, errs))
        )  
    )
;;

let impl name (resl: lookup) (_interp: ((context * Ast.craftenv) -> decl Seq.t -> craftsrc)) (_args: lit list) (block) = 
    (*fun  env and expression *)
    (fun (_env: craftenv) (_args': lit list) -> 
        (* resolve arguments with their local names (map params to vars) *)
        let e' = (
            _args'
            |> List.to_seq
            |> Seq.zip (List.to_seq _args) 
            |> Seq.fold_left (fun e' (argn, argv) -> 
                match (argn, argv) with
                | (VarIdent n, v) -> Env.define n v e'
                | _ -> e'
            ) _env
        ) in 

        (* run the interpreter! *)
        let e' = Env.spawn e' in
        let { prg=(Program({ errs; state; _ })); env } = _interp ({ state=[]; errs=[]; resl=resl }, e') (Seq.return block) in
        let env = Env.parent env in 

        match (state, errs) with
        | (((Stmt (Ret l)) :: _rest), []) -> 
            Ok (l, env)
        | (_, []) -> 
            Ok ((Nil), env)
        | (_, (_e :: _)) -> 
            Error (ErrGroup (name, errs))
    )
;;
