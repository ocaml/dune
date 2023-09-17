open Dune_action_plugin.V1

let action =
  let open O in
  let path_to_dependency = read_file ~path:(Path.of_string "foo_or_bar") in
  path_to_dependency
  |> stage ~f:(fun path_to_dependency ->
    let+ data = read_file ~path:(Path.of_string path_to_dependency) in
    print_endline data)
;;

let () = run action
