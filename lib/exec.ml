
let run (interp: char Seq.t) = 
    interp 
    |> Seq.iter (Format.print_char)
;;

let error line col message = 
    Format.sprintf "[line: %d, col: %d] error: %s" line col message 
;;
