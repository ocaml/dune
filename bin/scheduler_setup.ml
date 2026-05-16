open Import
open Scheduler

let maybe_clear_screen
      ~(terminal_persistence : Dune_config.Terminal_persistence.t)
      ~details_hum
  =
  match Execution_env.inside_dune with
  | true -> (* Don't print anything here to make tests less verbose *) ()
  | false ->
    (match terminal_persistence with
     | Clear_on_rebuild -> Console.reset ()
     | Clear_on_rebuild_and_flush_history -> Console.reset_flush_history ()
     | Preserve ->
       let message =
         sprintf
           "********** NEW BUILD (%s) **********"
           (String.concat ~sep:", " details_hum)
       in
       Console.print_user_message
         (User_message.make
            [ Pp.nop; Pp.tag User_message.Style.Success (Pp.verbatim message); Pp.nop ]))
;;

let on_event ~terminal_persistence = function
  | Run.Event.Tick -> Console.Status_line.refresh ()
  | Source_files_changed { details_hum } ->
    maybe_clear_screen ~terminal_persistence ~details_hum
  | Build_interrupted ->
    Console.Status_line.set
      (Live
         (fun () ->
           let progression =
             match Fiber.Svar.read Build_system.state with
             | Initializing
             | Restarting_current_build
             | Build_succeeded__now_waiting_for_changes
             | Build_failed__now_waiting_for_changes -> Build_system.Progress.init
             | Building progress -> progress
           in
           Pp.seq
             (Pp.tag User_message.Style.Error (Pp.verbatim "Source files changed"))
             (Pp.verbatim
                (sprintf
                   ", restarting current build... (%u/%u)"
                   progression.number_of_rules_executed
                   progression.number_of_rules_discovered))))
  | Build_finish build_result ->
    let message =
      match build_result with
      | Success -> Pp.tag User_message.Style.Success (Pp.verbatim "Success")
      | Failure ->
        let failure_message =
          match
            Build_system_error.(
              Id.Map.cardinal (Set.current (Fiber.Svar.read Build_system.errors)))
          with
          | 1 -> Pp.textf "Had 1 error"
          | n -> Pp.textf "Had %d errors" n
        in
        Pp.tag User_message.Style.Error failure_message
    in
    Console.Status_line.set
      (Constant (Pp.seq message (Pp.verbatim ", waiting for filesystem changes...")))
;;

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
      match Unix.kill (-Pid.to_int pid) Sys.sigterm with
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
  Run.go config ~on_event:(fun _ -> ()) f
;;

let go_without_rpc_server ~(common : Common.t) ~config:dune_config f =
  let config =
    let watch_exclusions = Common.watch_exclusions common in
    Dune_config.for_scheduler dune_config ~print_ctrl_c_warning:true ~watch_exclusions
  in
  Dune_rules.Clflags.concurrency := config.concurrency;
  let on_event = on_event ~terminal_persistence:dune_config.terminal_persistence in
  Run.go config ~on_event f
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

let go_with_rpc_server_and_console_status_reporting
      ~(common : Common.t)
      ~config:dune_config
      run
  =
  let rpc_server =
    match Common.rpc common with
    | `Allow server -> server
    | `Forbid_builds -> Code_error.raise "rpc must be enabled in polling mode" []
  in
  let server = rpc rpc_server in
  let config =
    let watch_exclusions = Common.watch_exclusions common in
    Dune_config.for_scheduler dune_config ~print_ctrl_c_warning:true ~watch_exclusions
  in
  Dune_rules.Clflags.concurrency := config.concurrency;
  let file_watcher = Common.file_watcher common in
  let run () =
    let open Fiber.O in
    Root.Rpc.Global.with_background_rpc server
    @@ fun () ->
    let* () = Root.Rpc.Global.ensure_ready () in
    with_action_runner_worker ~common run
  in
  Run.go
    config
    ~file_watcher
    ~on_event:(on_event ~terminal_persistence:dune_config.terminal_persistence)
    run
;;
