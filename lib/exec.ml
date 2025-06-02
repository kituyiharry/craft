let run (interp: string Seq.t) = 
    interp 
    |> Scanner.scan_lines
;;

let error line col message = 
    Format.sprintf "[line: %d, col: %d] error: %s" line col message 
;;
