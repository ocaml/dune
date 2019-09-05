open Dune_action

let action =
  read_file ~path:(Path.of_string "bar_source")
  |> stage ~f:(fun data -> write_file ~path:(Path.of_string "bar_copy") ~data)

let () = run action
