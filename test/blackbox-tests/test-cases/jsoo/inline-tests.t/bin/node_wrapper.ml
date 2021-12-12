let file = Sys.argv.(1)

let () = Printf.printf "Preparing node wrapper for %s" file
let (_ : int) = Sys.command (Printf.sprintf "node %s" file)
