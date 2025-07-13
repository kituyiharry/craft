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
        let { prg=(Program({ errs; state; _ })); env } = _interp ({ state=[]; errs=[]; resl=resl }, e') (Seq.return block) in

        match (state, errs) with
        | (((Stmt (Ret l)) :: _rest), []) -> 
            Ok (l, env)
        | (_, []) -> 
            Ok ((Nil), env)
        | (_, (_e :: _)) -> 
            Error (ErrGroup (name, errs))
    )
;;
