let runfile fname = 
    let _ = Format.print_newline () in
    let _ = Format.print_newline () in
    Craft.Io.fopen fname 
    |> Craft.Exec.run
    |> fst (* use snd to view errors *)
    |> List.map (fun (num, tok) -> 
        let _ = (let _ = Format.print_string (Format.sprintf "Line %d: \n" num)
        in
        List.iter (fun t -> 
            let _ = Format.printf "\t" in
            let _ = Format.print_string (Craft.Token.show_token t) in
            Format.print_newline ()
        ) tok ) 
        in (num, tok)
    )
    |> Craft.Exec.normalize
    |> Craft.Expr.parse
    |> Craft.Expr.show_expr
    |> function s -> 
        let _ = Format.print_newline () in
        Format.print_string s
;;

let main () = 
    if Array.length Sys.argv < 2 then
        Format.printf "usage: craft <script>\n"
    else if Array.length Sys.argv = 2 then
        let _ = Format.print_newline () in
        let _ = ignore @@ runfile Sys.argv.(1) in
        let _ = Format.print_newline () in
        Format.print_newline ()
    else
        ()
;;

let () = 
    (*let name = ref "" in *)
    (*let age  = ref 0 in *)
    (*let _ = Craft.Io.gets () "%s %d" (fun s i -> name := s; age := i) in*)
    main ()
;;
