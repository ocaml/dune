open Stdune
module Event = Dune_scheduler__Event
module Process_watcher = Dune_scheduler__Process_watcher
module Signal_watcher = Dune_scheduler__Signal_watcher
module Thread0 = Dune_scheduler__Thread0

let debug =
  match Sys.getenv_opt "DUNE_SIGNAL_REPRO_DEBUG" with
  | None -> fun _ -> ()
  | Some _ -> prerr_endline
;;

let env_int name ~default =
  match Sys.getenv_opt name with
  | None -> default
  | Some value -> Int.of_string value |> Option.value_exn
;;

let env_float name ~default =
  match Sys.getenv_opt name with
  | None -> default
  | Some value -> Float.of_string value |> Option.value_exn
;;

let sh =
  Bin.which "sh" ~path:(Env_path.path Env.initial)
  |> Option.map ~f:Path.to_string
  |> Option.value_exn
;;

let spawn_sleep command =
  let argv = [| sh; "-c"; command |] in
  Unix.create_process sh argv Unix.stdin Unix.stdout Unix.stderr |> Pid.of_int
;;

let reset_signal_mask () =
  let signos = List.map Thread0.interrupt_signals ~f:Signal.to_int in
  ignore (Unix.sigprocmask SIG_UNBLOCK signos : int list)
;;

let jobs_per_iteration = env_int "DUNE_SIGNAL_REPRO_JOBS" ~default:64
let iterations = env_int "DUNE_SIGNAL_REPRO_ITERS" ~default:100
let interrupt_after = env_float "DUNE_SIGNAL_REPRO_DELAY" ~default:0.005
let timeout_after = env_float "DUNE_SIGNAL_REPRO_TIMEOUT" ~default:5.0
let commands = [| "sleep 0.01"; "sleep 0.02"; "sleep 0.03"; "sleep 0.05" |]

type shutdown_mode =
  | Queue
  | Signal

let shutdown_mode =
  match Sys.getenv_opt "DUNE_SIGNAL_REPRO_SHUTDOWN" with
  | Some "signal" -> Signal
  | Some "queue" | None -> Queue
  | Some mode ->
    Code_error.raise "unknown signal repro shutdown mode" [ "mode", Dyn.string mode ]
;;

let cleanup_iteration events process_watcher =
  debug "starting iteration";
  let pids =
    List.init jobs_per_iteration ~f:(fun i ->
      spawn_sleep commands.(i mod Array.length commands))
  in
  List.iter pids ~f:(fun pid ->
    let job : Event.job =
      { pid; is_process_group_leader = false; ivar = Fiber.Ivar.create () }
    in
    Process_watcher.register_job process_watcher job);
  let (_ : Thread.t) =
    Thread.create
      (fun () ->
         Thread.delay interrupt_after;
         match shutdown_mode with
         | Queue -> Event.Queue.send_shutdown events (Signal Int)
         | Signal -> Unix.kill (Unix.getpid ()) Sys.sigint)
      ()
  in
  let rec wait_for_shutdown () =
    match Event.Queue.next events with
    | Shutdown (Signal Int) -> ()
    | Shutdown _ -> wait_for_shutdown ()
    | Job_complete_ready ->
      ignore (Process_watcher.wait_unix process_watcher : Fiber.fill list);
      wait_for_shutdown ()
    | Fiber_fill_ivar _
    | File_watcher_task _
    | Build_inputs_changed _
    | File_system_sync _
    | File_system_watcher_terminated -> wait_for_shutdown ()
  in
  wait_for_shutdown ();
  debug "got shutdown";
  Process_watcher.killall process_watcher Sys.sigkill;
  while Event.Queue.pending_jobs events > 0 do
    match Event.Queue.next events with
    | Shutdown _ -> ()
    | Job_complete_ready ->
      ignore (Process_watcher.wait_unix process_watcher : Fiber.fill list)
    | Fiber_fill_ivar _
    | File_watcher_task _
    | Build_inputs_changed _
    | File_system_sync _
    | File_system_watcher_terminated -> ()
  done
;;

let () =
  Printexc.record_backtrace true;
  try
    reset_signal_mask ();
    let events = Event.Queue.create () in
    debug "created queue";
    let signal_watcher = Signal_watcher.init ~print_ctrl_c_warning:false events in
    debug "started signal watcher";
    let finished = ref false in
    let (_ : Thread.t) =
      Thread.create
        (fun () ->
           Thread.delay timeout_after;
           if not !finished
           then (
             prerr_endline
               "signal watcher cleanup repro timed out while exercising SIGINT";
             exit 2))
        ()
    in
    let process_watcher = Process_watcher.init events in
    debug "started process watcher";
    for _ = 1 to iterations do
      cleanup_iteration events process_watcher
    done;
    debug "finished iterations";
    Unix.kill (Unix.getpid ()) (Signal.to_int Thread0.signal_watcher_interrupt);
    Thread0.join signal_watcher;
    finished := true;
    debug "joined signal watcher";
    Printf.printf "ok after %d iterations\n%!" iterations
  with
  | exn ->
    prerr_endline (Printexc.to_string exn);
    prerr_string (Printexc.get_backtrace ());
    exit 1
;;
