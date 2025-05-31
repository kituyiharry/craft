
(* Example: Reading lines from a file lazily 
   returns a sequence of srtings representing lines in the file 
*)
let fopen filename = 
    let ic = open_in filename in 
    (* memoize to maintain persistence *)
    Seq.of_dispenser (fun () -> In_channel.input_char ic)
;;

let gets () = 
  Seq.unfold (fun () ->
    try 
        Some (read_line (), ())
    with End_of_file -> None
  ) ()
;;
