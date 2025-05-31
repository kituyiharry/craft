(* attach a line number to each line *)
let map_index tok =  
    Seq.zip tok (Seq.ints 0)
;;
