open Dune_action_plugin.V1

let action =
  write_file ~path:(Path.of_string "../some_file") ~data:"Hello from some_file!"
;;

let () = run action
