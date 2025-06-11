let runfile fname = 
    let _ = Format.print_newline () in
    let _ = Format.print_newline () in
    Craft.Io.fopen fname 
    |> Craft.Exec.run
    |> (function {Craft.Scanner.errs;Craft.Scanner.toks} -> 
            let _ = 
                (List.iter (fun (lineno, errline) ->
                    List.iter (fun (colno, msg) -> 
                        Format.printf "Error lexing at line %d col %d: %s\n" lineno colno msg
                    ) errline
                ) errs)
            in toks
        )
    |> Craft.Exec.normalize
    |> Craft.Ast.parse
    |> Craft.Ast.show_expr
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
