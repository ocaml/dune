open Stdune
open Dune_action

let path = Path.of_string "bar1"

let action =
  read_file ~path
  |> stage ~f:(fun result ->
    let data = result |> Result.to_option |> Option.value_exn in
    write_file ~path ~data
    |> map ~f:(fun r -> r |> Result.to_option |> Option.value_exn))

let () = run action
