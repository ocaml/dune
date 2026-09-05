open Dune_rpc_lwt.V1.Action_plugin

let dependency =
  match Sys.argv with
  | [| _ |] -> "some_file1"
  | [| _; dependency |] -> dependency
  | _ -> invalid_arg "expected at most one dependency argument"
;;

let action dap =
  let open Lwt.Syntax in
  let* _ = read_file dap ~path:dependency in
  Lwt.return_unit
;;

let () = run action
