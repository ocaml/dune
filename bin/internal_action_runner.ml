open Stdune
open Import

(* dummy dispatcher of action runners based on the rules. we just use the path
   of the directory where the rule comes from. E.g.

   [$action_runner/foo/bar -> $action_runner] *)
let action_runner runners server =
  let action_runner_server = Dune_rpc_impl.Server.action_runner server in
  let runners =
    List.map runners ~f:(fun name ->
        Dune_engine.Action_runner.create action_runner_server ~name)
  in
  let module Action_runner = Dune_engine.Action_runner in
  Staged.stage @@ fun (input : Dune_engine.Action_exec.input) ->
  match
    Path.Source.of_string input.rule_loc.start.pos_fname |> Path.Source.explode
  with
  | [] -> None
  | runner :: _ ->
    List.find runners ~f:(fun r -> String.equal (Action_runner.name r) runner)

(* an all in one command to run a build with multiple action runners.

   1. We launch an action runner for every --runner argument
   2. We run the build and redirect some build commands to the action runners
   (depeneding on the rule directory)
   3. When the build is done, we wind down all the action runners
*)
let build =
  let name_ = Arg.info [] ~docv:"TARGET" in
  let+ common = Common.term
  and+ targets = Arg.(value & pos_all dep [] name_)
  and+ runners =
    Arg.(
      value & opt_all string [] & info [ "runner" ] ~docv:"NAME" ~doc:"runner.")
  in
  let config =
    let action_runner = action_runner runners in
    Common.init ~action_runner common
  in
  let targets =
    match targets with
    | [] -> [ Common.default_target common ]
    | _ :: _ -> targets
  in
  let request setup =
    Target.interpret_targets (Common.root common) config setup targets
  in
  Scheduler.go ~common ~config @@ fun () ->
  let open Fiber.O in
  let* () = Dune_engine.Rpc.ensure_ready () in
  let worker_pids =
    let prog =
      match Bin.which ~path:(Env_path.path Env.initial) Sys.argv.(0) with
      | Some p -> Path.to_string p
      | None -> User_error.raise [ Pp.text "unable to find dune in PATH" ]
    in
    let env =
      let env =
        Dune_rpc.Where.add_to_env (Dune_rpc_client.Where.default ()) Env.initial
      in
      Env.to_unix env |> Spawn.Env.of_list
    in
    List.map runners ~f:(fun name ->
        let argv =
          [ Sys.argv.(0); "internal"; "action-runner"; "start"; name ]
        in
        Spawn.spawn ~env ~prog ~argv () |> Pid.of_int)
  in
  Fiber.fork_and_join_unit
    (fun () ->
      Fiber.parallel_iter worker_pids ~f:(fun pid ->
          let+ _ = Scheduler.wait_for_process pid in
          ()))
    (fun () ->
      Fiber.finalize
        ~finally:(fun () ->
          List.iter worker_pids ~f:(fun pid ->
              Unix.kill (Pid.to_int pid) Sys.sigterm);
          Fiber.return ())
        (fun () ->
          let+ res = Build_cmd.run_build_system ~common ~request in
          match res with
          | Ok () -> ()
          | Error `Already_reported ->
            raise Dune_util.Report_error.Already_reported))

let start =
  let+ common = Common.term
  and+ name =
    Arg.(required & pos 0 (some string) None (Arg.info [] ~docv:"NAME"))
  in
  let common = Common.forbid_builds common in
  let config =
    let log_file =
      (* make sure that runners have their own individual logs *)
      let file =
        let fname = sprintf "%s.%d.log" name (Unix.getpid ()) in
        Path.build (Path.Build.relative Path.Build.root fname)
      in
      Log.File.This file
    in
    Common.init ~log_file common
  in
  Scheduler.go ~common ~config @@ fun () ->
  let where =
    match Dune_rpc.Where.of_env Env.initial with
    | Ok s -> s
    | Error `Missing ->
      User_error.raise
        [ Pp.textf "must set the environment variable %s" Dune_rpc.Where.env_var
        ]
    | Error (`Exn exn) ->
      (* TODO include actual value *)
      User_error.raise
        [ Pp.textf "the environment variable %s is invalid"
            Dune_rpc.Where.env_var
        ; Exn.pp exn
        ]
  in
  Dune_engine.Action_runner.Worker.start ~name ~where

let start = Cmd.v (Cmd.info "start") start

let build = Cmd.v (Cmd.info "build") build

let group = Cmd.group (Cmd.info "action-runner") [ start; build ]
