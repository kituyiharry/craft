module Env = struct 

    module ValEnv = Map.Make (String);;
    include ValEnv

    type t = Ast.lit ValEnv.t;; 

    let empty = ValEnv.empty;;

    (* we can overwrite vars *)
    let define name value env = 
        ValEnv.add name value env 
    ;;

    let get env name = 
        match ValEnv.find_opt name env with
        | Some x -> Ok x 
        | _ -> Error (Ast.Undefined name)
    ;;

    let assign env name value = 
        if ValEnv.mem name env then 
            Ok (ValEnv.add name value env)
        else
            Error (Ast.Undefined name)
    ;;

    let show env = 
        let sb = Buffer.create 100 in 
        let _ = Buffer.add_string sb "Env => { \n" in
        let _ = ValEnv.iter (fun k v -> 
            let _ = Buffer.add_string sb "    " in 
            let _ = Buffer.add_string sb k in 
            let _ = Buffer.add_string sb " => " in 
            let _ = Buffer.add_string sb (Ast.show_lit v) in 
            let _ = Buffer.add_string sb "\n" in 
            ()
        ) env in 
        let _ = Buffer.add_string sb "} \n" in
        Buffer.contents sb

end [@@deriving show];;
