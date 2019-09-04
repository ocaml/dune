open Dune_action

let action =
  write_file ~path:(Path.of_string "bar") ~data:"Hello from bar!"
  |> map ~f:(fun r -> r |> Result.get_ok)

let () = run action
