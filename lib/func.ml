open Env;;
open Ast;;

type interp = (craftenv -> expr -> (lit * craftenv, crafterr) result)

type funcobj = { 
        func: string      (* Function name  *)
    ;   envr: Env.t       (* Environment    *)
    ;   argl: lit list    (* argument list  *)
    ;   arty: int         (* function arity *)
    ;   blck: decl option (* declaration *)
};;

module Func = struct 

    let call (_eval: interp) (obj: funcobj)  = 
        match Env.get obj.envr obj.func with
        | Ok (FunImpl (arity, callable)) -> 
            if arity = obj.arty then
                (callable obj.envr obj.argl obj.blck)
            else
                Error (ArgMismatch (obj.func, arity, obj.arty))
        | Ok _ ->
            Error (Unimplmnted (Literal (VarIdent obj.func)))
        | Error e -> 
            Error e
    ;;

end
