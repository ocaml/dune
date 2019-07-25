open Dune_action_plugin.V1

let action =
  let open Dune_action_plugin.V1.O in
  let+ listing = read_directory ~path:(Path.of_string "some_dir") in
  String.concat "\n" listing |> print_endline

let () = run action
