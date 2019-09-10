open Dune_action_plugin

let action =
  let open Dune_action_plugin.O in
  let+ listing = read_directory ~path:(Path.of_string "foodir") in
  String.concat "\n" listing |> print_endline

let () = run action
