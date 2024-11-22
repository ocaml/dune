open! Import
module Pkg_dev_tool = Dune_rules.Pkg_dev_tool

let ocamllsp_exe_path = Path.build @@ Pkg_dev_tool.exe_path Ocamllsp
let ocamllsp_exe_name = Pkg_dev_tool.exe_name Ocamllsp

(* Replace the current dune process with ocamllsp. *)
let run_ocamllsp common ~args =
  let exe_path_string = Path.to_string ocamllsp_exe_path in
  Console.print_user_message
    (Dune_rules.Pkg_build_progress.format_user_message
       ~verb:"Running"
       ~object_:
         (User_message.command (String.concat ~sep:" " (ocamllsp_exe_name :: args))));
  Console.finish ();
  restore_cwd_and_execve common exe_path_string (exe_path_string :: args) Env.initial
;;

let build_ocamllsp common =
  let open Fiber.O in
  let+ result =
    Build_cmd.run_build_system ~common ~request:(fun _build_system ->
      Action_builder.path ocamllsp_exe_path)
  in
  match result with
  | Error `Already_reported -> raise Dune_util.Report_error.Already_reported
  | Ok () -> ()
;;

let is_in_dune_project builder =
  Workspace_root.create
    ~default_is_cwd:(Common.Builder.default_root_is_cwd builder)
    ~specified_by_user:(Common.Builder.root builder)
  |> Result.is_ok
;;

let go builder command =
  match is_in_dune_project builder with
  | false ->
    let verb =
      match command with
      | `Run_with_args _ -> "run"
      | `Install -> "install"
    in
    User_error.raise
      [ Pp.textf
          "Unable to %s %s as a dev-tool because you don't appear to be inside a dune \
           project."
          verb
          ocamllsp_exe_name
      ]
  | true ->
    let common, config = Common.init builder in
    Scheduler.go ~common ~config (fun () ->
      let open Fiber.O in
      let* () = Lock_dev_tool.lock_ocamllsp () |> Memo.run in
      let+ () = build_ocamllsp common in
      match command with
      | `Run_with_args args -> run_ocamllsp (Common.root common) ~args
      | `Install -> ())
;;

module Exec = struct
  let term =
    let+ builder = Common.Builder.term
    and+ args = Arg.(value & pos_all string [] (info [] ~docv:"ARGS")) in
    go builder (`Run_with_args args)
  ;;

  let info =
    let doc = "Run ocamllsp, installing it as a dev tool if necessary." in
    Cmd.info "ocamllsp" ~doc
  ;;

  let command = Cmd.v info term
end

module Install = struct
  let term =
    let+ builder = Common.Builder.term in
    go builder `Install
  ;;

  let info =
    let doc = "Install ocamllsp as a dev tool." in
    Cmd.info "ocamllsp" ~doc
  ;;

  let command = Cmd.v info term
end
