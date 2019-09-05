open Dune_action

let path = Path.of_string "bar2"

let action =
  write_file ~path ~data:"Hello from bar2!"
  |> stage ~f:(fun () -> read_file ~path |> map ~f:print_endline)

let () = run action
