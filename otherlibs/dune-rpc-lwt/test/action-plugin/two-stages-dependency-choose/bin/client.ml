open Dune_rpc_lwt.V1.Action_plugin

let () =
  print_endline "starting client";
  flush stdout
;;

let action dap =
  let open Lwt.Syntax in
  let* file = read_file dap ~path:"foo_or_bar" in
  let* data = read_file dap ~path:file in
  Lwt_io.printl data
;;

let () = run action
