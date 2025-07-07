open Ast;;
open Env;;


let clock _env _args = 
    Ok (Number (Unix.gettimeofday ()), _env) 
;;

let impl (_interp: ((context * Ast.craftenv) -> decl Seq.t -> craftsrc)) (_args: lit list) (block) = 
    (* env and expression *)
    (fun (_env: craftenv) (_args': lit list) -> 
        let e' = (
            _args'
            |> List.to_seq
            |> Seq.zip (List.to_seq _args) 
            |> Seq.fold_left (fun e' (argn, argv) -> 
                match (argn, argv) with
                | (VarIdent n, v) -> Env.define n v e'
                | _ -> 
                    let _ = Format.printf "Undefined var arg: %s -> %s" 
                    (show_lit argn) (show_lit argv)
                    in
                    e'
            ) _env
        ) in 

        let (r) = _interp ({ state=[]; errs=[] }, e') (Seq.return block) in
        Ok ((Nil), r.env)
    )
;;
