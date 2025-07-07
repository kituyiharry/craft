open Env;;
open Ast;;

(* interpreter is a function from environment and expression returning a literal
   and transformation of the environment - if any *)
type interp = (craftenv -> expr -> (lit * craftenv, crafterr) result)

type funcobj = { 
        func: string      (* Function name  *)
    ;   envr: Env.t       (* Environment    *)
    ;   argl: lit list    (* argument list  *)
    ;   arty: int         (* function arity *)
};;

module Func = struct 

    let call (obj: funcobj)  = 
        match Env.get obj.envr obj.func with
        | Ok (FunImpl (arity, callable)) -> 
            if arity = obj.arty then
                (callable obj.envr obj.argl)
            else
                Error (ArgMismatch (obj.func, arity, obj.arty))
        | Ok _ ->
            Error (Unimplmnted (Literal (VarIdent obj.func)))
        | Error e -> 
            Error e
    ;;

end
