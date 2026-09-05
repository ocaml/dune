open Dune_rpc_lwt.V1.Action_plugin

let action dap =
  let open Lwt.Syntax in
  let* data = read_file dap ~path:"some_absent_dependency" in
  Lwt_io.printl data
;;

let () =
  try Lwt_main.run (run action) with
  | Error.E message ->
    prerr_endline message;
    exit 1
;;
