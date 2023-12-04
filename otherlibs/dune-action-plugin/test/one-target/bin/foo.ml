open Dune_action_plugin.V1

let action =
  write_file ~path:(Path.of_string "some_target") ~data:"Hello from some_target!"
;;

let () = run action
