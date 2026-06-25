open Dune_rpc_lwt.V1.Action_plugin
module Glob = Dune_rpc_lwt.V1.Action_plugin.Glob

let action dap =
  let open Lwt.Syntax in
  let* _ = read_directory_with_glob dap ~glob:Glob.universal ~path:"." in
  Lwt.return_unit
;;

let () = run action
