let file = Sys.argv.(1)

let () = Printf.printf "I'm the runner for %s\n" file
let () = flush_all ()

let (_ : int) = Sys.command (Printf.sprintf "node %s" file)
