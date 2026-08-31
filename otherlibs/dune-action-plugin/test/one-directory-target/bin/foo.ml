open Dune_action_plugin.V1

let output =
  let nested =
    Directory.Builder.empty
    |> Directory.Builder.add_file ~name:"file.txt" ~data:"nested\n"
    |> Directory.Builder.build
  in
  Directory.Builder.empty
  |> Directory.Builder.add_file ~name:"root.txt" ~data:"root\n"
  |> Directory.Builder.add_directory ~name:"nested" ~directory:nested
  |> Directory.Builder.build
;;

let second =
  Directory.Builder.empty
  |> Directory.Builder.add_file ~name:"file.txt" ~data:"second\n"
  |> Directory.Builder.build
;;

let action =
  let open O in
  let+ () = write_directory ~path:(Path.of_string "output") ~directory:output
  and+ () = write_directory ~path:(Path.of_string "second") ~directory:second in
  ()
;;

let () = run action
