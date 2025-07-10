open Env;;
open Ast;;

(* interpreter is a function from environment and expression returning a literal
   and transformation of the environment - if any *)

type funcobj = { 
        func: string      (* Function name  *)
    ;   envr: Env.t       (* Environment    *)
    ;   argl: lit list    (* argument list  *)
    ;   arty: int         (* function arity *)
};;

module Func = struct 

    let call (obj: funcobj)  = 
        match Env.get obj.envr obj.func with
        | Ok (FunImpl (arity, closure, callable)) -> 
            (* use previously captured environment *)
            let env' = { env=closure.env; par=obj.envr.par } in
            if arity = obj.arty then
                let* l, r =  (callable env' obj.argl) in
                (* update closure environment!!  *)
                let r = Env.update obj.func (FunImpl (arity, r, callable)) r in 
                Ok(l, r)
            else
                Error (ArgMismatch (obj.func, arity, obj.arty))
        | Ok _ ->
            Error (Unimplmnted (Literal (VarIdent obj.func)))
        | Error e -> 
            Error e
    ;;

end
