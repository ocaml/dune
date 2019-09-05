open Dune_action

let action =
  let listing = read_directory ~path:(Path.of_string "foodir") in
  map listing ~f:(fun result -> String.concat "\n" result |> print_endline)

let () = run action
