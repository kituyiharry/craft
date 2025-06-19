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
    |> List.map (fun (_x, l) -> 
        let _ = List.iter (fun t -> 
            let s = Craft.Token.show_token t in 
            let _ = Format.print_newline () in
            Format.print_string s
        ) l in
        let _ = Format.print_newline () in
        (_x, l)
    )
    |> Craft.Exec.normalize
    |> Craft.Ast.parse
    |> (function 
        | Ok b -> 
            (let b' = (let _ = Format.print_newline () in
            let s = Craft.Ast.show_source b in 
            let _ = Format.print_string s in
            let _ = Format.print_newline () in
            let _ = Format.print_newline () in
            b) in b')
        | Error (expr, _rem) -> 
            (let b' = (
            let _ = Format.print_newline () in
            let s = Craft.Ast.show_expr expr in 
            let _ = Format.print_string (String.cat "Error!!! -> " s) in
            let _ = Format.print_newline () in
            let _ = Format.print_newline () in
                (Craft.Ast.Program {state=[];errs=[]})
            ) in b')
        )
    |> Craft.Eval.eval_exprs
    |> Craft.Ast.show_source
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
