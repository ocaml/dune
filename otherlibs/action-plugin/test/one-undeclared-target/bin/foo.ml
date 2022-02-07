open Dune_action_plugin.V1

let action = write_file ~path:(Path.of_string "bar") ~data:"Hello from bar!"
let () = run action
