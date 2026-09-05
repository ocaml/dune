open Dune_rpc_lwt.V1.Action_plugin

let () =
  run (fun dap ->
    let open Lwt.Syntax in
    let* data = read_file dap ~path:"input" in
    Lwt_io.print data)
;;
