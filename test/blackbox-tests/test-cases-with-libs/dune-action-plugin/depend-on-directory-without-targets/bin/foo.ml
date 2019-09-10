open Dune_action

let action =
  let open Dune_action.O in
  let+ listing = read_directory ~path:(Path.of_string "foodir") in
  String.concat "\n" listing |> print_endline

let () = run action
