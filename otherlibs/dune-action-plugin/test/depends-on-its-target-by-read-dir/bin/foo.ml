open Dune_action_plugin.V1
module Glob = Dune_action_plugin.V1.Glob

let action =
  read_directory_with_glob ~glob:Glob.universal ~path:(Path.of_string ".")
  |> map ~f:ignore
;;

let () = run action
