open Ast;;
let (let*) = Result.bind;;

module Env = struct 

    type t = craftenv;; 

    let empty = { env=ValEnv.empty; par=None; };;

    let show env = 
        let rec inshow tabcount env = 
            let sb = Buffer.create 200 in 
            let leadtab = 
                if (tabcount > 0) then 
                    (String.make tabcount ' ')
                else 
                    ""
            in
            let card = ValEnv.cardinal env.env in
            let _ = Buffer.add_string sb (
                leadtab ^ "(size: " ^ (Int.to_string card) ^ 
                " parent: " ^ (Bool.to_string (Option.is_some env.par)) ^ 
                ")\n") 
            in
            match env.par with
            | Some parenv -> 
                let _ = Buffer.add_string sb (leadtab ^ "Env => { \n") in
                let _ = ValEnv.iter (fun k v -> 
                    let _ = Buffer.add_string sb (leadtab ^ "    ") in 
                    let _ = Buffer.add_string sb (leadtab ^ k) in 
                    let _ = Buffer.add_string sb (leadtab ^ "=>") in 
                    let _ = Buffer.add_string sb (leadtab ^ (Ast.show_lit v)) in 
                    let _ = Buffer.add_string sb (leadtab ^ "\n") in 
                    ()
                ) env.env in 
                (*let _ = Buffer.add_string sb (leadtab ^ "{ \n") in*)
                let e' = inshow (tabcount + 1) parenv in
                let _ = Buffer.add_string sb e' in
                (*let _ = Buffer.add_string sb (leadtab ^ "} \n") in*)
                let _ = Buffer.add_string sb "} \n" in
                Buffer.contents sb
            | _ -> 
                let _ = Buffer.add_string sb (leadtab ^ "Env => { \n") in
                let _ = ValEnv.iter (fun k v -> 
                    let _ = Buffer.add_string sb (leadtab ^ "    ") in 
                    let _ = Buffer.add_string sb (leadtab ^ k) in 
                    let _ = Buffer.add_string sb (leadtab ^ " => ") in 
                    let _ = Buffer.add_string sb (leadtab ^ (Ast.show_lit v)) in 
                    let _ = Buffer.add_string sb (leadtab ^ "\n") in 
                    ()
                ) env.env in 
                let _ = Buffer.add_string sb (leadtab ^ "} \n") in
                Buffer.contents sb
        in inshow 0 env
    ;;


    (* we can overwrite vars *)
    let define name value env = 
        { env with env=(ValEnv.add name value env.env) } 
    ;;

    (* we can overwrite vars *)
    let rec update name value env = 
        match ValEnv.find_opt name env.env with
        | Some _ ->
            { env with env=(ValEnv.add name value env.env) }    
        | _ -> 
            (match env.par with
                | Some p ->
                    { env with par=(Some (update name value p)) }
                | None -> 
                    (* We didn't find that value *)
                    let _ = Format.printf "%s wasnt there\n" name in
                    raise Not_found
            )
    ;;

    let rec global env = 
        match env.par with 
        | None -> env.env 
        | Some p -> global p
    ;;

    let rec get env name = 
        match ValEnv.find_opt name env.env with
        | Some x -> Ok x 
        | _ -> 
            (match env.par with
            | Some par -> get par name
            | _ -> Error (Ast.Undefined name))
    ;;

    let rec get_at env name depth =   
        if depth = 0 then   
            let v = ValEnv.find name env.env in
            Ok (v)
        else
            let f = get_at (Option.get env.par) name (depth - 1)
            in 
            f
    ;;

    let fetch env resl name = 
        match Resolver.ScopeMap.find_opt name resl with
        | Some depth -> 
            get_at env name depth 
        | _ -> 
            Ok (ValEnv.find name (global env))
    ;;

    let rec assign env name value = 
        if ValEnv.mem name env.env then 
            Ok ({ env with env=(ValEnv.add name value env.env) })
        else 
            (match env.par with
            | Some par -> 
                let* par' = assign par name value in 
                Ok ({ env with par=(Some par') })
            | _ -> Error (Ast.Undefined name))
    ;;

    let rec assign_at env name value depth = 
        if depth = 0 then
            (* TODO: Check if available first! *)
            Ok ({ env with env=(ValEnv.add name value env.env) })
        else
            (match env.par with
                | Some par -> 
                    let* par' = assign_at par name value (depth - 1) in 
                    Ok ({ env with par=(Some par') })
                | _ -> Error (Ast.Undefined name))
    ;;

    let modify env name value resl = 
        match Resolver.ScopeMap.find_opt name resl with
        | Some depth -> 
            assign_at env name value depth 
        | _ -> 
            let rec modf env = 
                (match env.par with 
                    | None -> 
                        (* TODO: Was it even there!! *)
                        { env with env=(ValEnv.add name value env.env) }
                    | Some par -> 
                        { env with par=(Some (modf par)) }
                )
            in Ok (modf env)
    ;;

    let spawn env' = 
        { (empty) with par=(Some env') }
    ;;

    let parent {par;_} = 
        Option.get par 
    ;;
end [@@deriving show];;
