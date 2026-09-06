open Dune_rpc_lwt.V1.Action_plugin

let action dap =
  let open Lwt.Syntax in
  let* data = read_file dap ~path:"some_source" in
  Lwt_io.print data
;;

let () = run action
