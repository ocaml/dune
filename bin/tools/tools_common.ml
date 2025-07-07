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
    Build.run_build_system ~common ~request:(fun _build_system ->
      Action_builder.path (dev_tool_exe_path dev_tool))
  in
  match result with
  | Error `Already_reported -> raise Dune_util.Report_error.Already_reported
  | Ok () -> ()
;;

let build_dev_tool_via_rpc dev_tool =
  let target = dev_tool_build_target dev_tool in
  Build.build_via_rpc_server ~print_on_success:false ~targets:[ target ]
;;

let lock_and_build_dev_tool ~common ~config dev_tool =
  let open Fiber.O in
  match Dune_util.Global_lock.lock ~timeout:None with
  | Error _lock_held_by ->
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

let which_command dev_tool =
  let exe_path = dev_tool_exe_path dev_tool in
  let exe_name = Pkg_dev_tool.exe_name dev_tool in
  let term =
    let+ builder = Common.Builder.term
    and+ allow_not_installed =
      Arg.(
        value
        & flag
        & info
            [ "allow-not-installed" ]
            ~doc:
              (sprintf
                 "If %s is not installed as a dev tool, still print where it would be \
                  installed."
                 exe_name))
    in
    let _ : Common.t * Dune_config_file.Dune_config.t = Common.init builder in
    if allow_not_installed || Path.exists exe_path
    then print_endline (Path.to_string exe_path)
    else User_error.raise [ Pp.textf "%s is not installed as a dev tool" exe_name ]
  in
  let info =
    let doc =
      sprintf
        "Prints the path to the %s dev tool executable if it exists, errors out \
         otherwise."
        exe_name
    in
    Cmd.info exe_name ~doc
  in
  Cmd.v info term
;;
