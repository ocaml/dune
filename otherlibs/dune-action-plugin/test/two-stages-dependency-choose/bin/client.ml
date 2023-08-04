open Dune_action_plugin.V1

let action =
  let open Dune_action_plugin.V1.O in
  let switch = read_file ~path:(Path.of_string "foo_or_bar") in
  stage switch ~f:(fun file ->
    let+ data = read_file ~path:(Path.of_string file) in
    print_endline data)
;;

let () = run action
