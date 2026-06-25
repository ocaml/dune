open Dune_rpc_lwt.V1.Action_plugin

let action dap =
  let open Lwt.Syntax in
  let* source1 = read_file dap ~path:"source1" in
  let* source2 = read_file dap ~path:"source2" in
  Lwt_io.print (source1 ^ source2)
;;

let () = run action
