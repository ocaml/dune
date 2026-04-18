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
  { Dune_engine.Rpc.run = Dune_rpc_impl.Server.run server
  ; stop = Dune_rpc_impl.Server.stop server
  ; ready = Dune_rpc_impl.Server.ready server
  }
;;

let with_action_runner_worker ~(common : Common.t) f =
  match Common.action_runner common with
  | None -> f ()
  | Some runner ->
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
    let dune_prog =
      let prog = Sys.executable_name in
      if Filename.is_relative prog then find_in_path_exn prog else Path.of_string prog
    in
    let jobs = Int.to_string !Dune_rules.Clflags.concurrency in
    let env =
      Env.add Env.initial ~var:"DUNE_JOBS" ~value:jobs |> Env.to_unix |> Spawn.Env.of_list
    in
    let where = Dune_rpc_impl.Server.listening_address server in
    let worker_argv =
      [ Path.to_string dune_prog
      ; "internal"
      ; "action-runner"
      ; "start"
      ; Dune_engine.Action_runner.Name.to_string Common.action_runner_name
      ; Dune_rpc.Where.to_string where
      ]
    in
    let prog, argv =
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
    let run_build () =
      let* () = Dune_engine.Rpc.ensure_ready () in
      let trace_fd = Dune_trace.duplicate_global_fd () in
      let close_trace_fd () =
        Option.iter trace_fd ~f:(fun fd ->
          match Unix.close fd with
          | () -> ()
          | exception Unix.Unix_error (Unix.EBADF, _, _) -> ())
      in
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
          close_trace_fd ();
          let pid = Pid.of_int pid in
          Dune_trace.emit Action (fun () ->
            Dune_trace.Event.Action.runner_spawn
              ~name:
                (Dune_engine.Action_runner.Name.to_string
                   (Dune_engine.Action_runner.name runner))
              ~pid);
          pid
        | exception exn ->
          close_trace_fd ();
          raise exn
      in
      let terminate_worker () =
        match Unix.kill (-Pid.to_int pid) Sys.sigterm with
        | () -> Fiber.return ()
        | exception Unix.Unix_error (Unix.ESRCH, _, _) -> Fiber.return ()
      in
      let worker_exit = Fiber.Ivar.create () in
      let pool = Fiber.Pool.create () in
      let monitor_worker () =
        Fiber.Pool.task pool ~f:(fun () ->
          let timeout = Time.Span.of_secs 5.0 in
          let* status =
            Scheduler.wait_for_process pid ~timeout ~is_process_group_leader:true
          in
          let* () = Dune_engine.Action_runner.disconnect runner in
          Fiber.Ivar.fill worker_exit status)
      in
      Fiber.fork_and_join_unit
        (fun () -> Fiber.Pool.run pool)
        (fun () ->
           let* () = monitor_worker () in
           Fiber.finalize
             (fun () ->
                let* () = Dune_engine.Action_runner.await_ready runner in
                f ())
             ~finally:(fun () ->
               let* () = terminate_worker () in
               let* _status = Fiber.Ivar.read worker_exit in
               Fiber.Pool.close pool))
    in
    run_build ()
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
        Dune_engine.Rpc.with_background_rpc (rpc server) (fun () ->
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
  let server =
    match Common.rpc common with
    | `Allow server -> rpc server
    | `Forbid_builds -> Code_error.raise "rpc must be enabled in polling mode" []
  in
  let config =
    let watch_exclusions = Common.watch_exclusions common in
    Dune_config.for_scheduler dune_config ~print_ctrl_c_warning:true ~watch_exclusions
  in
  Dune_rules.Clflags.concurrency := config.concurrency;
  let file_watcher = Common.file_watcher common in
  let run () =
    let open Fiber.O in
    Dune_engine.Rpc.with_background_rpc server
    @@ fun () ->
    let* () = Dune_engine.Rpc.ensure_ready () in
    with_action_runner_worker ~common run
  in
  Run.go
    config
    ~file_watcher
    ~on_event:(on_event ~terminal_persistence:dune_config.terminal_persistence)
    run
;;
