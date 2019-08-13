open Stdune
open Dune_action

let action =
  let data = read_directory ~path:(Path.of_string "foodir") in
  map data ~f:(fun result ->
    result |> Result.to_option |> Option.value_exn |> String.concat ~sep:"\n"
    |> print_endline)

let () = run action
