open Dune_action

let action = read_file ~path:(Path.of_string "bar") |> map ~f:print_endline

let () = run action
