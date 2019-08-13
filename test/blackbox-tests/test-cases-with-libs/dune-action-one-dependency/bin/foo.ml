open Stdune
open Dune_action

let action =
  let data = read_file ~path:(Path.of_string "bar") in
  map data ~f:(fun result ->
    result |> Result.to_option |> Option.value_exn |> print_endline)

let () = run action
