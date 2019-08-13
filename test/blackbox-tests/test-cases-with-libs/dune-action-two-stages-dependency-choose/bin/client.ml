open Stdune
open Dune_action

let action =
  let switch = read_file ~path:(Path.of_string "foo_or_bar") in
  stage switch ~f:(fun file ->
    let file = file |> Result.to_option |> Option.value_exn in
    let data = read_file ~path:(Path.of_string file) in
    map data ~f:(fun data ->
      data |> Result.to_option |> Option.value_exn |> print_endline))

let () = run action
