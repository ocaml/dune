open! Stdune
open Fiber.O
module Action_runner = Dune_engine.Action_runner
module Rpc_server = Action_runner.Rpc_server
module Where = Dune_rpc_client.Where
module Scheduler = Dune_engine.Scheduler
module Server = Dune_rpc_server.Make (Csexp_rpc.Session)
module Action_exec = Dune_engine.Action_exec

let () = Dune_util.Log.init ~file:(Out_channel stderr) ()

let run () =
  let fname = "action-runner-test" in
  let where = `Unix fname in
  let action_runner_server = Action_runner.Rpc_server.create () in
  let csexp_server =
    match Csexp_rpc.Server.create (Where.to_socket where) ~backlog:10 with
    | Ok s ->
      at_exit (fun () -> Fpath.unlink_no_err fname);
      s
    | Error _ -> assert false
  in
  let handler =
    let handler =
      Dune_rpc_server.Handler.create ~version:(3, 7)
        ~on_init:(fun _ _ ->
          print_endline "server: client connected";
          Csexp_rpc.Server.stop csexp_server |> Fiber.return)
        ()
    in
    Action_runner.Rpc_server.implement_handler action_runner_server handler;
    handler
  in
  let server = Dune_rpc_server.make handler in
  let* sessions = Csexp_rpc.Server.serve csexp_server in
  let run_action_runner_server () = Server.serve sessions None server in
  let run_worker () =
    let name = "foo" in
    let* () = Csexp_rpc.Server.ready csexp_server in
    let pid =
      print_endline "running action runner";
      let env =
        let env = Dune_rpc_private.Where.add_to_env where Env.empty in
        Env.to_unix env |> Spawn.Env.of_list
      in
      let prog =
        Bin.which ~path:(Env_path.path Env.initial) "dune"
        |> Option.value_exn |> Path.to_absolute_filename
      in
      Spawn.spawn ~prog ~env
        ~argv:
          [ "dune"; "internal"; "action-runner"; "start"; "--root"; "."; name ]
        ()
      |> Pid.of_int
    in
    let worker = Action_runner.create action_runner_server ~name in
    Fiber.fork_and_join_unit
      (fun () ->
        print_endline "running action_runner_server";
        Fiber.fork_and_join_unit
          (fun () -> Action_runner.Rpc_server.run action_runner_server)
          (fun () ->
            let* status = Scheduler.wait_for_process pid in
            let+ () = Action_runner.Rpc_server.stop action_runner_server in
            let action_runner =
              sprintf "action_runner %s(pid=%d)" name (Pid.to_int pid)
            in
            match status.status with
            | WEXITED 0 -> ()
            | WEXITED n ->
              User_error.raise
                [ Pp.textf "%s exited with code %d" action_runner n ]
            | Unix.WSIGNALED s -> (
              match Signal.of_int s with
              | Term -> ()
              | signal ->
                User_error.raise
                  [ Pp.textf "%s exited with signal %s" action_runner
                      (Signal.name signal)
                  ])
            | Unix.WSTOPPED n ->
              User_error.raise
                [ Pp.textf "%s stopped with code %n" action_runner n ]))
      (fun () ->
        let action =
          let action = Dune_engine.Action.echo [ "foo" ] in
          { Action_exec.targets = None
          ; root = Path.root
          ; context = None
          ; env = Env.empty
          ; rule_loc = Loc.none
          ; execution_parameters =
              Dune_engine.Execution_parameters.builtin_default
          ; action
          }
        in
        let+ (_ : Action_exec.Exec_result.t) =
          Action_runner.exec_action worker action
        in
        print_endline "executed action";
        Unix.kill (Pid.to_int pid) Sys.sigterm)
  in
  Fiber.fork_and_join_unit run_action_runner_server run_worker

let%expect_test "run an action runner and dispatch one job to it" =
  let on_event _ _ = () in
  let config : Scheduler.Config.t =
    { concurrency = 2
    ; stats = None
    ; insignificant_changes = `Ignore
    ; signal_watcher = `No
    }
  in
  Scheduler.Run.go config ~timeout:2.0 ~on_event run;
  [%expect
    {|
    running action runner
    running action_runner_server
    server: client connected
    # RPC accepted the last client. No more clients will be accepted.
    executed action |}]
