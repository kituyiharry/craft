
  (*
   * In the book they use a trie to scan on demand, for us we will reuse the
   * scanner we already have :-)
   *
   *if (result == INTERPRET_COMPILE_ERROR) exit(65);
   *if (result == INTERPRET_RUNTIME_ERROR) exit(70);
   *
   *)

  let repl () = 
      let buf = Buffer.create 1024 in
      let _   = Printf.printf "=== CraftVM v0.1.0 (Toy language) ===\n 
      \rUse CTRL-C to close the REPL\n" in
      let rec bufstream () = 
          let _ = Printf.printf ">>> %!" in
          let _ =
              Seq.of_dispenser (fun () -> In_channel.input_char In_channel.stdin)
              |> Seq.take_while ((!=)'\n')
              |> Seq.iter (Buffer.add_char buf) 
          in 
          let size = Buffer.length buf in 
          if size < 1 then () else 
          Buffer.contents buf 
          |> Seq.return 
          |> Exec.run
          |> (function {Scanner.errs;Scanner.toks} -> 
              match errs with 
              | [] -> 
                  let _ = List.iter (fun (_, tl) -> 
                      List.iter (fun {Token.ttype;_} -> 
                          Printf.printf "| %s\n%!" (Token.show_tokentype ttype) 
                      ) tl
                  ) toks in
                  let _ = Format.print_newline () in
                  let _ = Buffer.clear buf in
                  bufstream ()
              | errs -> 
                  let _ = 
                      (List.iter (fun (lineno, errline) ->
                          List.iter (fun (colno, msg) -> 
                              Format.printf "Error lexing at line %d col %d: %s\n" lineno colno msg
                          ) errline
                      ) errs)
                  in 
                  let _ = Buffer.clear buf in
                  bufstream ()
          )
      in bufstream ()
  ;;

