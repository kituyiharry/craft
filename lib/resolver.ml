module ScopeMap = Map.Make (String);;
type scope  = (bool ScopeMap.t) [@opaque] ;;
type lookup = (int ScopeMap.t)  [@opaque] ;;

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
    type t = { 
            scopes: (scope list) [@opaque]
        ;   locals: (lookup)     [@opaque]
    } [@@deriving show];;

    let empty = { scopes=[]; locals=ScopeMap.empty }
    ;;

    let begin_scope ({ scopes=res;_ } as v: t) = 
        ({ v with scopes=(ScopeMap.empty :: res) })
    ;;

    let declare name ({ scopes=res;_ } as v: t)= 
        match res with 
        | locals :: rest -> 
            ({ v with scopes=(ScopeMap.add name false locals) :: rest }) 
        | _ -> v
    ;;

    let define name ({ scopes=res; _ } as v: t) = 
        match res with 
        | locals :: rest -> 
            ({ v with scopes=(ScopeMap.add name true locals) :: rest }) 
        | _ -> v
    ;;

    let end_scope ({ scopes=res; _ } as v: t) = { v with scopes = List.tl res }
    ;;

    let show (res: t) = 
        let sb = Buffer.create 200 in 
        let _  = Buffer.add_string sb "Scopes: -> [  " in
        let leadtab = " " in
        let _ = ScopeMap.iter (fun k v -> 
            let _ = Buffer.add_string sb (leadtab ^ (k)) in 
            let _ = Buffer.add_string sb (leadtab ^ (" => ")) in 
            let _ = Buffer.add_string sb (leadtab ^ ((Int.to_string v))) in 
            ()
        ) res.locals in 
        let _ = Buffer.add_string sb "  ]" in
        Buffer.contents sb
    ;;

end

let show_lookup t =
    Resolver.show { Resolver.empty with locals=t }
;;

let pp_lookup (_f) t = 
    Format.printf "%s" (Resolver.show { Resolver.empty with locals=t })
;;
