open Dune_rpc_lwt.V1.Action_plugin

let action dap =
  let open Lwt.Syntax in
  let* data = read_file dap ~path:"some_dependency" in
  Lwt_io.printl data
;;

let () = run action
