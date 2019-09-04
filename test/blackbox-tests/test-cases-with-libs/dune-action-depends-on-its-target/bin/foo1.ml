open Dune_action

let path = Path.of_string "bar1"

let action =
  read_file ~path
  |> stage ~f:(fun result ->
    let data = result |> Result.get_ok in
    write_file ~path ~data |> map ~f:(fun r -> r |> Result.get_ok))

let () = run action
