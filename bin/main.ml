let runfile fname = 
    Craft.Io.fopen fname 
    |> Craft.Exec.run
;;

let main () = 
    if Array.length Sys.argv < 2 then
        Format.printf "usage: craft <script>\n"
    else if Array.length Sys.argv = 2 then
        runfile Sys.argv.(1)
    else
        ()
;;

let () = 
    (*let name = ref "" in *)
    (*let age  = ref 0 in *)
    (*let _ = Craft.Io.gets () "%s %d" (fun s i -> name := s; age := i) in*)
    main ()
;;
