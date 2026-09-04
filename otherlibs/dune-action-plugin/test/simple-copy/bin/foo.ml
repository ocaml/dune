open Dune_action_plugin.V1

let action = read_file ~path:(Path.of_string "some_source") |> map ~f:print_string
let () = run action
