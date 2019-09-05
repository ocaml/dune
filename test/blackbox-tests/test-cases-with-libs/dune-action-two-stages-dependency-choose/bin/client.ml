open Dune_action

let action =
  let switch = read_file ~path:(Path.of_string "foo_or_bar") in
  stage switch ~f:(fun file ->
    read_file ~path:(Path.of_string file) |> map ~f:print_endline)

let () = run action
