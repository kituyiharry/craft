let runfile fname = 
    let _ = Format.pp_print_newline Format.std_formatter () in
    Craft.Io.fopen fname 
    |> Craft.Exec.run
    |> List.iter (fun res -> 
        (match res with
            | Ok (num, tok) -> 
                let _ = Format.pp_print_string Format.std_formatter (Format.sprintf "Line %d: \n" num)
                in
                List.iter (fun t -> 
                    let _ = Format.printf "\t" in
                    let _ = Craft.Token.pp_token Format.std_formatter t in
                    Format.pp_print_newline Format.std_formatter ()
                ) tok 
            | Error (num, e) ->
                Format.printf "Error at line %d: %s\n" num e)
    )
;;

let main () = 
    if Array.length Sys.argv < 2 then
        Format.printf "usage: craft <script>\n"
    else if Array.length Sys.argv = 2 then
        ignore @@ runfile Sys.argv.(1)
    else
        ()
;;

let () = 
    (*let name = ref "" in *)
    (*let age  = ref 0 in *)
    (*let _ = Craft.Io.gets () "%s %d" (fun s i -> name := s; age := i) in*)
    main ()
;;
