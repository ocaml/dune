open Dune_action

let path = Path.of_string "bar1"

let action = read_file ~path |> stage ~f:(fun data -> write_file ~path ~data)

let () = run action
