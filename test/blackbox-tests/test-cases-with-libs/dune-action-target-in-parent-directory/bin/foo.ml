open Dune_action

let action = write_file ~path:(Path.of_string "../bar") ~data:"Hello from bar!"

let () = run action
