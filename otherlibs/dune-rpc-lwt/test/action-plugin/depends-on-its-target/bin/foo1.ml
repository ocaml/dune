open Dune_rpc_lwt.V1.Action_plugin

let path = "some_file1"

let action dap =
  let open Lwt.Syntax in
  let* _ = read_file dap ~path in
  Lwt.return_unit
;;

let () = run action
