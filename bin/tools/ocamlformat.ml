open! Import
module Pkg_dev_tool = Dune_rules.Pkg_dev_tool

let exe_path = Path.build @@ Pkg_dev_tool.exe_path Ocamlformat
let exe_name = Pkg_dev_tool.exe_name Ocamlformat

let run_dev_tool workspace_root ~args =
  let exe_path_string = Path.to_string exe_path in
  Console.print_user_message
    (Dune_rules.Pkg_build_progress.format_user_message
       ~verb:"Running"
       ~object_:(User_message.command (String.concat ~sep:" " (exe_name :: args))));
  Console.finish ();
  restore_cwd_and_execve
    workspace_root
    exe_path_string
    (exe_path_string :: args)
    Env.initial
;;

let dev_tool_exe_exists () = Path.exists exe_path

let build_dev_tool common =
  match dev_tool_exe_exists () with
  | true ->
    (* Avoid running the build system if the executable already exists
       to reduce unnecessary latency in the common case. *)
    Fiber.return ()
  | false ->
    let open Fiber.O in
    let+ result =
      Build_cmd.run_build_system ~common ~request:(fun _build_system ->
        Action_builder.path exe_path)
    in
    (match result with
     | Error `Already_reported -> raise Dune_util.Report_error.Already_reported
     | Ok () -> ())
;;

module Exec = struct
  let term =
    let+ builder = Common.Builder.term
    and+ args = Arg.(value & pos_all string [] (info [] ~docv:"ARGS")) in
    let common, config = Common.init builder in
    Scheduler.go ~common ~config (fun () ->
      let open Fiber.O in
      let* () = Lock_dev_tool.lock_ocamlformat () |> Memo.run in
      let+ () = build_dev_tool common in
      run_dev_tool (Common.root common) ~args)
  ;;

  let info =
    let doc =
      {|Wrapper for running ocamlformat intended to be run automatically
       by a text editor. All positional arguments will be passed to the
       ocamlformat executable (pass flags to ocamlformat after the '--'
       argument, such as 'dune ocamlformat -- --help').|}
    in
    Cmd.info "ocamlformat" ~doc
  ;;

  let command = Cmd.v info term
end

module Which = struct
  let term =
    let+ builder = Common.Builder.term in
    let _ : Common.t * Dune_config_file.Dune_config.t = Common.init builder in
    print_endline (Path.to_string exe_path)
  ;;

  let info =
    let doc = {|Prints the path to the ocamlformat binary.|} in
    Cmd.info "ocamlformat" ~doc
  ;;

  let command = Cmd.v info term
end
