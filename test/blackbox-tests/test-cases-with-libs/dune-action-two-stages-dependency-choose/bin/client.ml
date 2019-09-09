open Dune_action

let action =
  let open Dune_action.O in
  let switch = read_file ~path:(Path.of_string "foo_or_bar") in
  stage switch ~f:(fun file ->
      let+ data = read_file ~path:(Path.of_string file) in
      print_endline data)

let () = run action
