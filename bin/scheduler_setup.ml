open Import
open Scheduler

let rpc server =
  { Root.Rpc.Global.run = Dune_rpc_impl.Server.run server
  ; stop = Dune_rpc_impl.Server.stop server
  ; ready = Dune_rpc_impl.Server.ready server
  }
;;

let with_action_runner_worker ~(common : Common.t) f =
  match Common.action_runner common with
  | None -> f ()
  | Some action_runner ->
    let open Fiber.O in
    let server =
      match Common.rpc common with
      | `Allow server -> server
      | `Forbid_builds -> Code_error.raise "action runners require the dune RPC server" []
    in
    let find_in_path_exn prog =
      match Bin.which ~path:(Env_path.path Env.initial) prog with
      | Some path -> path
      | None -> User_error.raise [ Pp.textf "unable to find %s in PATH" prog ]
    in
    let has_directory_component prog =
      String.exists prog ~f:(function
        | '/' | '\\' -> true
        | _ -> false)
    in
    let dune_prog =
      let prog = Sys.executable_name in
      if Filename.is_relative prog && not (has_directory_component prog)
      then find_in_path_exn prog
      else Path.of_filename_relative_to_initial_cwd prog
    in
    let jobs = Int.to_string !Dune_rules.Clflags.concurrency in
    let env =
      Env.initial
      |> Env.add ~var:"DUNE_JOBS" ~value:jobs
      |> Env.add ~var:"DUNE_BUILD_DIR" ~value:(Path.Build.to_string Path.Build.root)
      |> Env.to_unix
      |> Spawn.Env.of_list
    in
    let where = Dune_rpc_impl.Server.listening_address server in
    let monitor_pool = Fiber.Pool.create () in
    let started_worker = ref (None : Pid.t option) in
    let worker_command runner ~generation =
      let worker_argv =
        [ Path.to_string dune_prog
        ; "internal"
        ; "action-runner"
        ; "start"
        ; Action_runner_name.to_string (Dune_engine.Action_runner.name runner)
        ; Int.to_string generation
        ; Dune_rpc.Where.to_string where
        ]
      in
      if Common.sandbox_actions common
      then (
        let bwrap =
          match Platform.OS.value with
          | Linux -> find_in_path_exn "bwrap"
          | _ ->
            User_error.raise
              [ Pp.text "--sandbox-actions is currently only supported on Linux" ]
        in
        let cwd = Path.to_absolute_filename Path.root in
        let shared_cache_bindings =
          let build_cache_dir = Lazy.force Dune_cache.Layout.build_cache_dir in
          Path.mkdir_p build_cache_dir;
          let build_cache_dir = Path.to_string build_cache_dir in
          [ "--ro-bind"; build_cache_dir; build_cache_dir ]
        in
        ( Path.to_string bwrap
        , [ Path.to_string bwrap; "--die-with-parent"; "--bind"; "/"; "/" ]
          @ shared_cache_bindings
          @ [ "--proc"; "/proc"; "--dev"; "/dev"; "--chdir"; cwd; "--" ]
          @ worker_argv ))
      else Path.to_string dune_prog, worker_argv
    in
    let close_trace_fd trace_fd = Option.iter trace_fd ~f:Fd.close in
    let start_worker ~runner ~generation =
      let* () = Root.Rpc.Global.ensure_ready () in
      let prog, argv = worker_command runner ~generation in
      let trace_fd = Dune_trace.duplicate_global_fd () in
      let argv =
        argv
        @
        match trace_fd with
        | None -> []
        | Some fd -> [ "--trace-fd"; Int.to_string (Fd.unsafe_to_int fd) ]
      in
      let pid =
        match Spawn.spawn ~env ~prog ~argv ~setpgid:Spawn.Pgid.new_process_group () with
        | pid ->
          close_trace_fd trace_fd;
          let pid = Pid.of_int pid in
          Dune_trace.emit Action (fun () ->
            Dune_trace.Event.Action.runner_spawn
              ~name:(Dune_engine.Action_runner.name runner)
              ~pid);
          pid
        | exception exn ->
          close_trace_fd trace_fd;
          raise exn
      in
      started_worker := Some pid;
      Fiber.Pool.task monitor_pool ~f:(fun () ->
        let* _status = Scheduler.wait_for_process pid ~is_process_group_leader:true in
        let* () = Dune_engine.Action_runner.disconnect runner ~generation in
        (match !started_worker with
         | Some current_pid when Pid.equal current_pid pid -> started_worker := None
         | None | Some _ -> ());
        Fiber.return ())
    in
    let terminate_worker pid =
      let pid = Pid.to_int pid in
      let pid = if Sys.win32 then pid else -pid in
      match Unix.kill pid Sys.sigterm with
      | () -> Fiber.return ()
      | exception Unix.Unix_error (Unix.ESRCH, _, _) -> Fiber.return ()
    in
    Dune_engine.Action_runner.set_start action_runner start_worker;
    Fiber.fork_and_join_unit
      (fun () -> Fiber.Pool.run monitor_pool)
      (fun () ->
         Fiber.finalize f ~finally:(fun () ->
           let worker = !started_worker in
           let* () =
             match worker with
             | None -> Fiber.return ()
             | Some pid -> terminate_worker pid
           in
           Fiber.Pool.close monitor_pool))
;;

let no_build_no_rpc ~config:dune_config f =
  let config =
    Dune_config.for_scheduler dune_config ~print_ctrl_c_warning:true ~watch_exclusions:[]
  in
  Dune_rules.Clflags.concurrency := config.concurrency;
  Run.go config f
;;

let go_without_rpc_server ~(common : Common.t) ~config:dune_config f =
  let config =
    let watch_exclusions = Common.watch_exclusions common in
    Dune_config.for_scheduler dune_config ~print_ctrl_c_warning:true ~watch_exclusions
  in
  Dune_rules.Clflags.concurrency := config.concurrency;
  Run.go config f
;;

let go_with_rpc_server ~common ~config f =
  let f =
    match Common.rpc common with
    | `Allow server ->
      fun () ->
        Root.Rpc.Global.with_background_rpc (rpc server) (fun () ->
          with_action_runner_worker ~common f)
    | `Forbid_builds -> f
  in
  go_without_rpc_server ~common ~config f
;;

let go_with_rpc_server_and_file_watcher
      ~(common : Common.t)
      ~config:dune_config
      ~rpc_server
      run
  =
  let config =
    let watch_exclusions = Common.watch_exclusions common in
    Dune_config.for_scheduler dune_config ~print_ctrl_c_warning:true ~watch_exclusions
  in
  Dune_rules.Clflags.concurrency := config.concurrency;
  let file_watcher = Common.file_watcher common in
  let run () =
    let server = rpc rpc_server in
    Root.Rpc.Global.with_background_rpc server
    @@ fun () ->
    Fiber.fork_and_join_unit Root.Rpc.Global.ensure_ready (fun () ->
      with_action_runner_worker ~common run)
  in
  Run.go config ~file_watcher run
;;
