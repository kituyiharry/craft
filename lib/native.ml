open Ast;;

let clock _env _args _dcl = 
    Ok (Number (Unix.gettimeofday ()), _env) 
;;


