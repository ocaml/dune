open Import

let name = Action_runner_name.of_string "action-runner"

let socketpair () =
  if Sys.win32
  then User_error.raise [ Pp.text "action runners are not supported on Windows" ];
  let parent_fd, worker_fd = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let parent_fd = Fd.unsafe_of_unix_file_descr parent_fd in
  let worker_fd = Fd.unsafe_of_unix_file_descr worker_fd in
  Fd.set_close_on_exec parent_fd;
  Fd.clear_close_on_exec worker_fd;
  parent_fd, worker_fd
;;

let create ~config ~sandbox_actions =
  let parent_fd, worker_fd = socketpair () in
  let parent_fd_transferred = ref false in
  Exn.protect
    ~finally:(fun () ->
      Fd.close worker_fd;
      if not !parent_fd_transferred then Fd.close parent_fd)
    ~f:(fun () ->
      let pid =
        let env =
          let jobs =
            let scheduler_config =
              Dune_config.for_scheduler
                config
                ~watch_exclusions:[]
                ~print_ctrl_c_warning:true
            in
            Int.to_string scheduler_config.concurrency
          in
          Env.add Env.initial ~var:"DUNE_JOBS" ~value:jobs
          |> Env.add ~var:"DUNE_BUILD_DIR" ~value:(Path.Build.to_string Path.Build.root)
          |> Env.to_unix
          |> Spawn.Env.of_list
        in
        let trace_fd = Dune_trace.duplicate_global_fd () in
        let prog, argv =
          let dune_prog = Util.dune_executable () in
          let worker_argv =
            [ Path.to_string dune_prog
            ; "internal"
            ; "action-runner"
            ; "start"
            ; Action_runner_name.to_string name
            ]
          in
          if sandbox_actions
          then (
            let { Bwrap.prog; argv } =
              Bwrap.wrap ~cwd:(Path.to_absolute_filename Path.root) worker_argv
            in
            prog, argv)
          else Path.to_string dune_prog, worker_argv
        in
        let argv =
          argv
          @ [ "--rpc-fd"; Int.to_string (Fd.unsafe_to_int worker_fd) ]
          @
          match trace_fd with
          | Some fd -> [ "--trace-fd"; Int.to_string (Fd.unsafe_to_int fd) ]
          | None -> []
        in
        Exn.protect
          ~finally:(fun () -> Option.iter trace_fd ~f:Fd.close)
          ~f:(fun () ->
            (* Use SIGTERM because the action runner knows how to clean up
               its children. *)
            Spawn.spawn ~env ~prog ~argv ~pdeathsig:Term ())
      in
      let action_runner =
        Dune_engine.Action_runner.create name pid ~connection_fd:parent_fd
      in
      parent_fd_transferred := true;
      action_runner)
;;

let inherit_trace_fd ~name trace_fd =
  Option.iter trace_fd ~f:(fun fd ->
    if Sys.win32 then Code_error.raise "trace fd handoff is not supported on Windows" [];
    Dune_trace.set_global_inherited_fd
      ~common_args:[ "action_runner", Sexp.Atom (Action_runner_name.to_string name) ]
      fd)
;;

let start_worker ~name ~rpc_fd ~trace_fd =
  inherit_trace_fd ~name trace_fd;
  Dune_engine.Action_runner_worker.start ~name ~rpc_fd
;;
