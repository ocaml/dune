open Dune_action_plugin.V1

let path = Path.of_string "some_file2"
let action = read_file ~path |> map ~f:ignore
let () = run action
