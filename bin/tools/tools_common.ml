open! Import
module Pkg_dev_tool = Dune_rules.Pkg_dev_tool

let dev_tool_exe_path dev_tool = Path.build @@ Pkg_dev_tool.exe_path dev_tool

let dev_tool_build_target dev_tool =
  Dune_lang.Dep_conf.File
    (Dune_lang.String_with_vars.make_text
       Loc.none
       (Path.to_string (dev_tool_exe_path dev_tool)))
;;

let build_dev_tool_directly common dev_tool =
  let open Fiber.O in
  let+ result =
    Build_cmd.run_build_system ~common ~request:(fun _build_system ->
      Action_builder.path (dev_tool_exe_path dev_tool))
  in
  match result with
  | Error `Already_reported -> raise Dune_util.Report_error.Already_reported
  | Ok () -> ()
;;

let build_dev_tool_via_rpc dev_tool =
  let target = dev_tool_build_target dev_tool in
  Build_cmd.build_via_rpc_server ~print_on_success:false ~targets:[ target ]
;;

let lock_and_build_dev_tool ~common ~config dev_tool =
  let open Fiber.O in
  match Dune_util.Global_lock.lock ~timeout:None with
  | Error () ->
    Scheduler.go_without_rpc_server ~common ~config (fun () ->
      let* () = Lock_dev_tool.lock_dev_tool dev_tool |> Memo.run in
      build_dev_tool_via_rpc dev_tool)
  | Ok () ->
    Scheduler.go_with_rpc_server ~common ~config (fun () ->
      let* () = Lock_dev_tool.lock_dev_tool dev_tool |> Memo.run in
      build_dev_tool_directly common dev_tool)
;;

let run_dev_tool workspace_root dev_tool ~args =
  let exe_name = Pkg_dev_tool.exe_name dev_tool in
  let exe_path_string = Path.to_string (dev_tool_exe_path dev_tool) in
  Console.print_user_message
    (Dune_rules.Pkg_build_progress.format_user_message
       ~verb:"Running"
       ~object_:(User_message.command (String.concat ~sep:" " (exe_name :: args))));
  Console.finish ();
  restore_cwd_and_execve workspace_root exe_path_string args Env.initial
;;

let lock_build_and_run_dev_tool ~common ~config dev_tool ~args =
  lock_and_build_dev_tool ~common ~config dev_tool;
  run_dev_tool (Common.root common) dev_tool ~args
;;
