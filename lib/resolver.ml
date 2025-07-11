module Resolver = struct 

    (*
        The scope stack is only used for local block scopes. 
        Variables declared at the top level in the global scope are not tracked
        by the resolver since they are more dynamic in Lox

        the bool value is used to track whether a variable is in its own
        initializer so that we can avoid. If false then its "not initialized"
        therefore in its own initializer, if true then it is "initialized". We
        take the former as an Error!
    *)

    module ScopeMap = Map.Make (String);;


    type scope = { 
        locals: (bool ScopeMap.t) [@opaque] 
    } [@@deriving show];;

    type t     = (scope list) [@@deriving show];;

    let empty = []

    let begin_scope (res: t) = 
        ({ locals=ScopeMap.empty } :: res)
    ;;

    let declare (res: t) name = 
        match res with 
        | { locals } :: rest -> 
            { locals=(ScopeMap.add name false locals); } :: rest 
        | _ -> res
    ;;

    let define (res: t) name = 
        match res with 
        | { locals } :: rest -> 
            { locals=(ScopeMap.add name true locals); } :: rest 
        | _ -> res
    ;;

    let end_scope = List.tl
    ;;

    let show (res: t) = 

            let sb = Buffer.create 200 in 
            let _  = Buffer.add_string sb "Scopes: -> [  " in
            let leadtab = "===>  " in
            let _ = List.iter (fun { locals } -> 
                let _ = Buffer.add_string sb (leadtab ^ "locals => { \n") in
                let _ = ScopeMap.iter (fun k v -> 
                    let _ = Buffer.add_string sb ("\t") in 
                    let _ = Buffer.add_string sb (k) in 
                    let _ = Buffer.add_string sb (" => ") in 
                    let _ = Buffer.add_string sb ((Bool.to_string v)) in 
                    let _ = Buffer.add_string sb ("\n") in 
                    ()
                ) locals in 
                Buffer.add_string sb (leadtab ^ "} \n")
            ) res in
            let _  = Buffer.add_string sb "  ]" in
            Buffer.contents sb

end ;;
