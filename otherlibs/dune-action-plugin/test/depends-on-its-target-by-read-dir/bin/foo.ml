open Dune_action_plugin.V1
module Glob = Dune_glob.V1

let action =
  let open Dune_action_plugin.V1.O in
  let+ _ = read_directory_with_glob ~glob:Glob.universal ~path:(Path.of_string ".")
  and+ _ = write_file ~path:(Path.of_string "some_file") ~data:"Hello from some_file!" in
  ()
;;

let () = run action
