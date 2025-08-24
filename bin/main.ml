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
        (_x, l))
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
                (Craft.Ast.Program {state=[];errs=[]; resl=(Craft.Resolver.ScopeMap.empty)})
            ) in b')
        )
    |> Craft.Eval.eval_exprs
    (*|> function {prg=s; env=e} -> *)
        (*let _ = Format.print_newline () in*)
        (*let _ = Format.print_string (Craft.Env.Env.show e) in*)
        (*s*)
    (*|> Craft.Ast.show_source*)
    (*|> function s -> *)
        (*let _ = Format.print_newline () in*)
        (*Format.print_string s*)
;;

let runfilevm fname = 
    let _ = Format.print_newline () in
    let _ = Format.print_newline () in
    Craft.Io.fopen fname 
    |> Craft.Exec.run
    |> (function {Craft.Scanner.errs;Craft.Scanner.toks} -> 
        match errs with 
        | [] -> 
            (*print_lex_tokens toks;*)
            let tseq' = Craft.Exec.normalize toks in 
            Craft.Craftvm.compile (Seq.append tseq' (Seq.return (Craft.Token.EOF, 0, 0)))
        | errs -> 
            Craft.Repl.print_lex_errs errs;
            InterpretCompileError
    )

;;

let main () = 
    (*let _ = Format.printf "vm: %s\n" (Craft.Craftvm.hello_world (Seq.return (Craft.Token.DOT))) in*)
    if Array.length Sys.argv == 1 then
        Craft.Repl.repl ()
    else if Array.length Sys.argv = 2 then
        let _ = Format.print_newline () in
        let _ = ignore @@ runfile Sys.argv.(1) in
        let _ = Format.print_newline () in
        Format.print_newline ()
    else if Array.length Sys.argv > 2 then 
        (if String.equal Sys.argv.(2) "--vm" then
            let _ = Format.printf "Starting VM" in
            ignore @@ runfilevm Sys.argv.(1) 
        else
            ignore @@ runfile Sys.argv.(1)
        )
    else
        Format.printf "usage: craft <script> <optional: --vm\n"
;;

let () = 
    (*let name = ref "" in *)
    (*let age  = ref 0 in *)
    (*let _ = Craft.Io.gets () "%s %d" (fun s i -> name := s; age := i) in*)
    main ()
;;
