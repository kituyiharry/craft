
(*
   * In the book they use a trie to scan on demand, for us we will reuse the
   * scanner we already have :-)
   *
   *if (result == INTERPRET_COMPILE_ERROR) exit(65);
   *if (result == INTERPRET_RUNTIME_ERROR) exit(70);
   *
   *)

let print_lex_tokens toks = 
    List.iter (fun (_, tl) -> 
        List.iter (fun {Token.ttype;_} -> 
            Printf.printf "| %s\n%!" (Token.show_tokentype ttype) 
        ) tl
    ) toks 
;;

let print_lex_errs errs = 
    List.iter (fun (lineno, errline) ->
        List.iter (fun (colno, msg) -> 
            Printf.printf "Error lexing at line %d col %d: %s\n%!" lineno colno msg
        ) errline
    ) errs
;;

let print_parse_exprs exprs = 
    List.iter (fun x -> 
        Printf.printf "%s\n%!" (Ast.show_expr x)
    ) exprs
;;

let print_token_output tseq = 
    let curl = ref 0 in
    let _  = Printf.printf "" in
    let _  = Format.printf "0 | " in
    let _  = Seq.iter (fun (t, l, _c) ->   
        if !curl = l then
            Format.printf " %s " (Token.show_tokentype t)
        else
            curl := l;
            Format.printf "\n %d | %s " (l) (Token.show_tokentype t)
    ) tseq in 
    Format.printf "\n"
;;

let repl () = 
    let buf = Buffer.create 1024 in
    let _   = Printf.printf "=== CraftVM v0.1.0 (Toy language) ===\n 
      \rUse CTRL-C or 'quit' or 'exit' to close the REPL\n" in
    let rec bufstream tseq lineno () = 
        let _ = Printf.printf ">>> %!" in
        let _ =
            Seq.of_dispenser (fun () -> In_channel.input_char In_channel.stdin)
            |> Seq.take_while ((!=)'\n')
            |> Seq.iter (Buffer.add_char buf) 
        in 
        let size = Buffer.length buf in 
        if size < 1 then
            (match Ast.parse tseq with
                | Ok (Program({errs;_}) as p)  -> 
                    (match errs with 
                        | [] -> 
                            let ptext = Ast.show_source p in 
                            let _ = Printf.printf "%s\n%!" ptext in 
                            bufstream tseq (lineno) ()
                        | e  -> 
                            let _ = Printf.printf "Parse Errors: " in 
                            let _ = print_parse_exprs e in
                            bufstream tseq (lineno) ()
                    )
                | Error e -> 
                    Printf.printf "ParseError: %s\n%!" (Ast.show_crafterr e)
            )
        else 
            let bufcont = Buffer.contents buf in
            if String.equal bufcont "quit" || String.equal bufcont "exit" then 
                (Printf.printf "Goodbye! :-) %!") 
            else if String.equal bufcont "!showtokens"  then
                ( 
                    (*print_token_output tseq;*)
                    let _ = Craftvm.compile tseq in
                    Buffer.clear buf;
                    bufstream tseq (lineno) () 
                )
            else
                Buffer.contents buf 
                |> Seq.return 
                |> Exec.run
                |> (function {Scanner.errs;Scanner.toks} -> 
                    match errs with 
                    | [] -> 
                        (*print_lex_tokens toks;*)
                        let tseq' = Exec.normalize toks
                            |> Seq.map (fun (t, _l, c) -> (t, lineno, c)) 
                            |> Seq.append tseq in 
                        (*Printf.print_newline ();*)
                        Buffer.clear buf;
                        bufstream tseq' (lineno + 1) ()
                    | errs -> 
                        print_lex_errs errs;
                        let _ = Buffer.clear buf in
                        bufstream tseq  (lineno) ()
                    )
    in bufstream Seq.empty 0 ()
;;

