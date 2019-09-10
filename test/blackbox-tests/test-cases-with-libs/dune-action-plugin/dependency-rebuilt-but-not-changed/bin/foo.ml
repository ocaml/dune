open Dune_action_plugin

let action =
  let open Dune_action_plugin.O in
  let+ data = read_file ~path:(Path.of_string "bar") in
  print_endline data

let () = run action
