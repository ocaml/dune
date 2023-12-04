open Dune_action_plugin.V1

let action =
  read_file ~path:(Path.of_string "some_source")
  |> stage ~f:(fun data -> write_file ~path:(Path.of_string "some_copy") ~data)
;;

let () = run action
