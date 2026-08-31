open Dune_action_plugin.V1

let directory =
  Directory.Builder.empty
  |> Directory.Builder.add_file ~name:"placeholder" ~data:""
  |> Directory.Builder.build
;;

let action =
  write_directory ~path:(Path.of_string "output") ~directory
  |> stage ~f:(fun () ->
    write_file ~path:(Path.of_string "output/some_target") ~data:"Hello from some_target!")
;;

let () = run action
