open Dune_rpc_lwt.V1.Action_plugin
module Glob = Dune_rpc_lwt.V1.Action_plugin.Glob

let action dap =
  let open Lwt.Syntax in
  let glob = Glob.of_string "some_file*" in
  let* listing = read_directory_with_glob dap ~path:"some_dir" ~glob in
  Lwt_io.printl (String.concat "\n" listing)
;;

let () = run action
