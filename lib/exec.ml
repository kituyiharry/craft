let run (interp: string Seq.t) = 
    interp 
    |> Scanner.scan_lines
;;

let normalize lext =
    lext
    |> List.to_seq
    |> Seq.map (fun (lineno, toks) -> 
        List.to_seq toks 
        |> Seq.map (fun {Token.ttype;Token.col} -> (ttype, lineno, col))
    )
    |> Seq.concat
;;
