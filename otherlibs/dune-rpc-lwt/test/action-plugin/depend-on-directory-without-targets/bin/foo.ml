open Dune_rpc_lwt.V1.Action_plugin
module Glob = Dune_rpc_lwt.V1.Action_plugin.Glob

let action dap =
  let open Lwt.Syntax in
  let* listing = read_directory_with_glob dap ~glob:Glob.universal ~path:"some_dir" in
  Lwt_io.printf "Directory listing: [%s]" (String.concat "; " listing)
;;

let () = run action
