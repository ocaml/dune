open Dune_action

let action =
  let data = read_file ~path:(Path.of_string "bar") in
  map data ~f:(fun result -> result |> Result.get_ok |> print_endline)

let () = run action
