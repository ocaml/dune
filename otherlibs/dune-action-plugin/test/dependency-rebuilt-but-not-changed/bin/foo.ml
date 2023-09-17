open Dune_action_plugin.V1

let action =
  let open Dune_action_plugin.V1.O in
  let+ data = read_file ~path:(Path.of_string "some_file") in
  print_endline data
;;

let () = run action
