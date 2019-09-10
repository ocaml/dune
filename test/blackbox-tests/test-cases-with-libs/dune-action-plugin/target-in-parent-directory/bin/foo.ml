open Dune_action_plugin

let action = write_file ~path:(Path.of_string "../bar") ~data:"Hello from bar!"

let () = run action
