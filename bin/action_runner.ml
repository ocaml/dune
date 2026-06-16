open Import

let name = Action_runner_name.of_string "action-runner"

let find_in_path_exn prog =
  match Bin.which ~path:(Env_path.path Env.initial) prog with
  | Some path -> path
  | None -> User_error.raise [ Pp.textf "unable to find %s in PATH" prog ]
;;

let has_directory_component prog =
  String.exists prog ~f:(function
    | '/' | '\\' -> true
    | _ -> false)
;;

let dune_prog () =
  let prog = Sys.executable_name in
  if Filename.is_relative prog && not (has_directory_component prog)
  then find_in_path_exn prog
  else Path.of_filename_relative_to_initial_cwd prog
;;

let create rpc_server ~config ~sandbox_actions =
  let rpc = Dune_rpc_impl.Server.action_runner rpc_server in
  let dune_prog = dune_prog () in
  let worker_argv =
    [ Path.to_string dune_prog
    ; "internal"
    ; "action-runner"
    ; "start"
    ; Action_runner_name.to_string name
    ]
  in
  let prog, argv =
    if sandbox_actions
    then (
      let { Bwrap.prog; argv } =
        Bwrap.wrap ~cwd:(Path.to_absolute_filename Path.root) worker_argv
      in
      prog, argv)
    else Path.to_string dune_prog, worker_argv
  in
  let where =
    Dune_rpc.Where.to_string (Dune_rpc_impl.Server.listening_address rpc_server)
  in
  let trace_fd = Dune_trace.duplicate_global_fd () in
  let argv =
    argv
    @ [ where ]
    @
    match trace_fd with
    | Some fd -> [ "--trace-fd"; Int.to_string (Fd.unsafe_to_int fd) ]
    | None -> []
  in
  let scheduler_config =
    Dune_config.for_scheduler config ~watch_exclusions:[] ~print_ctrl_c_warning:true
  in
  let env =
    let jobs = Int.to_string scheduler_config.concurrency in
    Env.initial
    |> Env.add ~var:"DUNE_JOBS" ~value:jobs
    |> Env.add ~var:"DUNE_BUILD_DIR" ~value:(Path.Build.to_string Path.Build.root)
    |> Env.to_unix
    |> Spawn.Env.of_list
  in
  let pid =
    Exn.protect
      ~f:(fun () -> Spawn.spawn ~env ~prog ~argv ~setpgid:Spawn.Pgid.new_process_group ())
      ~finally:(fun () -> Option.iter trace_fd ~f:Fd.close)
    |> Pid.of_int_exn
  in
  Dune_trace.emit Action (fun () ->
    Dune_trace.Event.Action.Runner.runner_event
      ~name
      (Dune_trace.Event.Action.Runner.Spawn pid));
  let worker = { Dune_engine.Action_runner.Worker.pid; is_process_group_leader = true } in
  Dune_engine.Action_runner.create rpc ~name ~worker
;;

let parse_name name = Action_runner_name.parse_string_exn (Loc.none, name)

let parse_where where =
  match
    Dune_rpc.Conv.of_sexp
      Dune_rpc.Where.sexp
      ~version:Dune_rpc.Version.latest
      (Sexp.Atom where)
  with
  | Ok where -> where
  | Error err ->
    User_error.raise
      [ Pp.textf "invalid action runner RPC address %S" where
      ; Pp.text (Dyn.to_string (Dune_rpc.Conv.dyn_of_error err))
      ]
;;

let inherit_trace_fd ~name trace_fd =
  Option.iter trace_fd ~f:(fun fd ->
    if Sys.win32 then Code_error.raise "trace fd handoff is not supported on Windows" [];
    Dune_trace.set_global_inherited_fd
      ~common_args:[ "action_runner", Sexp.Atom (Action_runner_name.to_string name) ]
      (Fd.unsafe_of_int (Int.of_string_exn fd)))
;;

let start_worker ~name ~where ~trace_fd =
  let name = parse_name name in
  inherit_trace_fd ~name trace_fd;
  let where = parse_where where in
  Dune_engine.Action_runner_worker.start ~name ~where
;;
