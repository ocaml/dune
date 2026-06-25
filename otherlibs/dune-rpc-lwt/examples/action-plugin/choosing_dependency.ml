open Dune_rpc_lwt.V1.Action_plugin

let action dap =
  let open Lwt.Syntax in
  let* path_to_dependency = read_file dap ~path:"foo_or_bar" in
  let* data = read_file dap ~path:path_to_dependency in
  Lwt_io.printl data
;;

let () = run action
