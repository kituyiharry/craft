open Env;;
open Ast;;
open Resolver;;

(* interpreter is a function from environment and expression returning a literal
   and transformation of the environment - if any *)

type funcobj = { 
        func: string      (* Function name  *)
    ;   envr: Env.t       (* Environment    *)
    ;   scop: int ScopeMap.t
    ;   argl: lit list    (* argument list  *)
    ;   arty: int         (* function arity *)
};;

module Func = struct 

    let call (obj: funcobj)  = 
        (*match Env.get obj.envr obj.func with*)
        match Env.fetch obj.envr obj.scop obj.func with
        | Ok (FunImpl (arity, _closure, callable)) -> 
            (* use previously captured environment *)
            (*let env' = { env=closure.env; par=obj.envr.par } in*)
            if arity = obj.arty then
                let* l, r =  (callable obj.envr obj.argl) in
                (* update closure environment!!  *)
                (*let _ = Format.printf "updating in %s\n" (Env.show r) in*)
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
