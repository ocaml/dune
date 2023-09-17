open Dune_action_plugin.V1
module Glob = Dune_glob.V1

let action =
  let open Dune_action_plugin.V1.O in
  let+ listing =
    read_directory_with_glob ~glob:Glob.universal ~path:(Path.of_string "some_dir")
  in
  String.concat "; " listing |> Printf.printf "Directory listing: [%s]"
;;

let () = run action
