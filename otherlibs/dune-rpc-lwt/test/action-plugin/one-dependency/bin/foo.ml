open Dune_rpc_lwt.V1.Action_plugin

let ordinary_action dap =
  let open Lwt.Syntax in
  let* data = read_file dap ~path:"some_dependency" in
  Lwt_io.printl data
;;

let write_connection path =
  let output = open_out path in
  output_string output (Sys.getenv "DUNE_DYNAMIC_RUN_ACTION_ID");
  output_char output '\n';
  output_string output (Sys.getenv "DUNE_RPC");
  output_char output '\n';
  close_out output
;;

let held_action _dap ~connection ~release =
  let open Lwt.Syntax in
  let* () =
    Lwt_io.with_file ~mode:Output "held-target" (fun output -> Lwt_io.write output "held")
  in
  write_connection connection;
  while not (Sys.file_exists release) do
    Unix.sleepf 0.05
  done;
  Lwt.return_unit
;;

let action dap =
  match Sys.argv with
  | [| _ |] -> ordinary_action dap
  | [| _; "hold"; connection; release |] -> held_action dap ~connection ~release
  | [| _; "steal" |] -> Lwt.return_unit
  | _ -> invalid_arg "invalid arguments"
;;

let () = run action
