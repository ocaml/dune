open Dune_action

let action =
  let data = read_directory ~path:(Path.of_string "foodir") in
  map data ~f:(fun result ->
    result |> Result.get_ok |> String.concat "\n" |> print_endline)

let () = run action
