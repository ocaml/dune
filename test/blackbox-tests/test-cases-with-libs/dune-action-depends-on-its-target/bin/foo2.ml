open Dune_action

let path = Path.of_string "bar2"

let action =
  write_file ~path ~data:"Hello from bar2!"
  |> stage ~f:(fun result ->
    let () = result |> Result.get_ok in
    read_file ~path |> map ~f:(fun r -> r |> Result.get_ok |> print_endline))

let () = run action
