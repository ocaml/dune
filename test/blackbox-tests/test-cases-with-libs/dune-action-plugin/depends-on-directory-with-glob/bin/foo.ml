open Dune_action_plugin

let action =
  let open Dune_action_plugin.O in
  let glob = Glob.of_string "some_file*" in
  let+ listing =
    read_directory_with_glob ~path:(Path.of_string "some_dir") ~glob
  in
  String.concat "\n" listing |> print_endline

let () = run action
