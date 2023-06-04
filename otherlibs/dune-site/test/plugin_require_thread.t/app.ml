(* load all the available plugins *)
let () =
  try
    Sites.Plugins.Plugins.load_all ()
  with exn ->
    Printf.printf "Error during dynamic linking: %s" (Printexc.to_string exn)

let () = print_endline "Main app starts..."
(* Execute the code registered by the plugins *)
let () = Queue.iter (fun f -> f ()) Registration.todo
