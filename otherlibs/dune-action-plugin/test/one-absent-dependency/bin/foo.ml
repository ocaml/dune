open Dune_action_plugin.V1

let action =
  let open Dune_action_plugin.V1.O in
  let+ data = read_file ~path:(Path.of_string "some_absent_dependency") in
  print_endline data
;;

let () = run action
