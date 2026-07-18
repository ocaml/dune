open Import
open Fiber.O

module Config = struct
  type t =
    { concurrency : int
    ; print_ctrl_c_warning : bool
    ; watch_exclusions : string list
    }
end

let spawn_thread ~name f = Thread0.spawn ~name f

include Types.Scheduler

external set_child_subreaper : unit -> bool = "dune_scheduler_set_child_subreaper"

let child_subreaper_enabled =
  lazy
    (match Platform.OS.value with
     | Platform.OS.Linux -> set_child_subreaper ()
     | _ -> false)
;;

let running_jobs_count (t : t) = Process_watcher.running_count t.process_watcher

exception Build_cancelled

let cancelled () = raise (Memo.Non_reproducible Build_cancelled)

let check_cancelled = function
  | Some cancel when Fiber.Cancel.fired cancel -> cancelled ()
  | None | Some _ -> ()
;;

let check_point =
  let* () = Fiber.return () in
  match t_opt () with
  | None -> Fiber.return ()
  | Some t ->
    (* CR-someday amokhov: we used to call [check_cancelled t] here but that led
       to a significant performance regression. Raising [Build_cancelled] saves
       some unnecessary recomputation but also destroys early cutoffs. We should
       change Memo to store previous successes to make such early cancellations
       preserve the early cutoff behaviour. *)
    Event.Queue.yield_if_there_are_pending_events t.events
;;

let () = Memo.check_point := check_point

let with_job_slot ?cancellation f =
  let* () = Fiber.return () in
  let t = t () in
  Fiber.Throttle.run t.job_throttle ~f:(fun () ->
    check_cancelled cancellation;
    f ())
;;

let wait_for_process t ~is_process_group_leader pid =
  let ivar = Fiber.Ivar.create () in
  Process_watcher.register_job t.process_watcher { pid; is_process_group_leader; ivar };
  let+ proc_info = Fiber.Ivar.read ivar in
  if Pid.Set.mem t.preserved_child_processes proc_info.pid
  then
    t.preserved_child_processes
    <- Pid.Set.remove t.preserved_child_processes proc_info.pid;
  proc_info
;;

(* Grace period before escalating from SIGTERM to SIGKILL *)
let sigterm_grace_period = Time.Span.of_secs 0.2

type termination_reason =
  | Normal
  | Cancel
  | Timeout

let child_process_poll_interval = Time.Span.of_secs 0.01

let preserve_child_process pid =
  Option.iter (t_opt ()) ~f:(fun t ->
    t.preserved_child_processes <- Pid.Set.add t.preserved_child_processes pid)
;;

let trace_child_process_cleanup pids stage =
  Dune_trace.emit Process (fun () ->
    let pids = Pid.Set.to_list pids in
    Dune_trace.Event.child_process_cleanup ~pids stage)
;;

let warn_child_process_cleanup_failed pids paragraphs =
  trace_child_process_cleanup pids `Failed;
  User_warning.emit
    ([ Pp.text "Unable to clean up subreaper child processes." ]
     @ paragraphs
     @ [ Pp.text "Continuing without failing the current operation." ])
;;

let child_process_cleanup_candidates t =
  let preserved_child_processes =
    Pid.Set.union
      t.preserved_child_processes
      (Process_watcher.running_pids t.process_watcher)
  in
  match Proc.Linux.Process_tree.children_of (Pid.me ()) with
  | Error _ as error -> error
  | Ok pids -> Ok (Pid.Set.diff pids preserved_child_processes)
;;

let child_process_cleanup_candidates_after_reap t =
  match child_process_cleanup_candidates t with
  | Error _ as error -> error
  | Ok pids when Pid.Set.is_empty pids -> Ok pids
  | Ok pids ->
    Pid.Set.iter pids ~f:(fun pid ->
      ignore (Proc.wait (Proc.Pid pid) [ WNOHANG ] : Proc.Process_info.t option));
    child_process_cleanup_candidates t
;;

let signal_processes pids signal =
  if Pid.Set.is_empty pids
  then Ok ()
  else (
    trace_child_process_cleanup pids (`Sent_signal signal);
    Pid.Set.fold pids ~init:(Ok ()) ~f:(fun pid acc ->
      match acc with
      | Error _ -> acc
      | Ok () ->
        (match Pid.kill pid `Pid signal with
         | `Delivered | `Dead -> Ok ()
         | exception exn ->
           Error
             [ Pp.textf
                 "failed to signal child process %d with SIG%s"
                 (Pid.to_int pid)
                 (Signal.name signal)
             ; Exn.pp exn
             ])))
;;

let rec await_no_child_processes t ~deadline ~signal ~signaled =
  match child_process_cleanup_candidates_after_reap t with
  | Error error -> Fiber.return (`Failed [ Proc.Linux.Process_tree.pp_error error ])
  | Ok pids when Pid.Set.is_empty pids -> Fiber.return `Exited
  | Ok pids when Time.(now () >= deadline) -> Fiber.return (`Timed_out pids)
  | Ok pids ->
    let unsignaled = Pid.Set.diff pids signaled in
    (match signal_processes unsignaled signal with
     | Error paragraphs -> Fiber.return (`Failed paragraphs)
     | Ok () ->
       Async_io.Task.await (Async_io.sleep t.async_io child_process_poll_interval)
       >>= (function
        | Ok () ->
          let signaled = Pid.Set.union signaled unsignaled in
          await_no_child_processes t ~deadline ~signal ~signaled
        | Error (`Exn _) -> assert false
        | Error `Cancelled ->
          (match child_process_cleanup_candidates_after_reap t with
           | Ok pids -> Fiber.return (`Timed_out pids)
           | Error error ->
             Fiber.return (`Failed [ Proc.Linux.Process_tree.pp_error error ]))))
;;

let cleanup_subreaper_child_processes_impl t =
  match child_process_cleanup_candidates_after_reap t with
  | Error error ->
    warn_child_process_cleanup_failed
      Pid.Set.empty
      [ Proc.Linux.Process_tree.pp_error error ];
    Fiber.return ()
  | Ok pids when Pid.Set.is_empty pids -> Fiber.return ()
  | Ok pids ->
    trace_child_process_cleanup pids `Started;
    let deadline = Time.add (Time.now ()) sigterm_grace_period in
    await_no_child_processes t ~deadline ~signal:Term ~signaled:Pid.Set.empty
    >>= (function
     | `Failed paragraphs ->
       warn_child_process_cleanup_failed pids paragraphs;
       Fiber.return ()
     | `Exited ->
       trace_child_process_cleanup pids `Finished;
       Fiber.return ()
     | `Timed_out _ ->
       let deadline = Time.add (Time.now ()) sigterm_grace_period in
       await_no_child_processes t ~deadline ~signal:Kill ~signaled:Pid.Set.empty
       >>| (function
        | `Failed paragraphs -> warn_child_process_cleanup_failed pids paragraphs
        | `Exited -> trace_child_process_cleanup pids `Finished
        | `Timed_out pids ->
          warn_child_process_cleanup_failed
            pids
            [ Pp.text "child processes remained alive after SIGTERM and SIGKILL"
            ; Pp.textf
                "child processes still alive: %s"
                (Pid.Set.to_list pids
                 |> List.map ~f:(fun pid -> Pid.to_int pid |> Int.to_string)
                 |> String.concat ~sep:", ")
            ]))
;;

let cleanup_subreaper_child_processes () =
  if Lazy.force child_subreaper_enabled
  then (
    let t = t () in
    cleanup_subreaper_child_processes_impl t)
  else Fiber.return ()
;;

let terminate_process_group =
  let trace_process_group_cleanup pid stage =
    Dune_trace.emit Process (fun () -> Dune_trace.Event.process_group_cleanup ~pid stage)
  in
  let process_group_cleanup_after_exit_enabled =
    (* Once an action's process exits, its pid can be reused as a pgid. Only
       use pgid-based cleanup after process exit when pid reuse during the grace
       period is unlikely. *)
    let process_group_cleanup_after_exit_min_pid_max = 4_000_000 in
    lazy
      (match Platform.OS.value with
       | Platform.OS.Linux ->
         (match Proc.Linux.read_pid_max () with
          | Some pid_max -> pid_max >= process_group_cleanup_after_exit_min_pid_max
          | None -> false)
       | _ -> false)
  in
  let process_group_poll_interval = Time.Span.of_secs 0.01 in
  let await_process_group_exit t pid =
    (* After sending SIGTERM once, poll with signal 0 instead of sending SIGTERM
       again. A process may use its SIGTERM handler for cleanup, and repeatedly
       running that handler during the grace period can interfere with that
       cleanup. *)
    let deadline = Time.add (Time.now ()) sigterm_grace_period in
    let rec loop () =
      match Pid.check pid `Group with
      | `Dead -> Fiber.return `Exited
      | `Alive ->
        if Time.(now () >= deadline)
        then Fiber.return `Timed_out
        else
          Async_io.Task.await (Async_io.sleep t.async_io process_group_poll_interval)
          >>= (function
           | Ok () -> loop ()
           | Error `Cancelled -> Fiber.return `Timed_out
           | Error (`Exn _) -> assert false)
    in
    let+ res = loop () in
    let () =
      match res with
      | `Timed_out -> trace_process_group_cleanup pid (`Timed_out sigterm_grace_period)
      | `Exited -> trace_process_group_cleanup pid `Finished
    in
    res
  in
  fun t pid ~is_process_group_leader ->
    if
      (not is_process_group_leader)
      || not (Lazy.force process_group_cleanup_after_exit_enabled)
    then Fiber.return ()
    else (
      match Pid.kill pid `Group Term with
      | `Dead ->
        trace_process_group_cleanup pid `Already_exited;
        Fiber.return ()
      | `Delivered ->
        trace_process_group_cleanup pid (`Sent_signal Term);
        await_process_group_exit t pid
        >>= (function
         | `Exited -> Fiber.return ()
         | `Timed_out ->
           (match Pid.kill pid `Group Kill with
            | `Dead -> Fiber.return ()
            | `Delivered ->
              trace_process_group_cleanup pid (`Sent_signal Kill);
              await_process_group_exit t pid
              >>| (function
               | `Exited -> ()
               | `Timed_out ->
                 User_error.raise
                   [ Pp.textf "failed to terminate process group %d" (Pid.to_int pid)
                   ; Pp.text "Processes remained alive after SIGTERM and SIGKILL."
                   ; Pp.text
                       "This can happen if an action is repeatedly spawning new \
                        processes."
                   ]))))
;;

(* We use this version privately in this module whenever we can pass the
   scheduler explicitly *)
let wait_for_build_process t ?cancellation ~is_process_group_leader pid =
  let sigkill_alarm = ref None in
  let wait () =
    let* r = wait_for_process t ~is_process_group_leader pid in
    let* () = terminate_process_group t pid ~is_process_group_leader in
    let+ () =
      match !sigkill_alarm with
      | None -> Fiber.return ()
      | Some alarm -> Async_io.Task.cancel alarm
    in
    r
  in
  match cancellation with
  | None ->
    let+ res = wait () in
    res, Normal
  | Some cancel ->
    let+ res, outcome =
      Fiber.Cancel.with_handler
        cancel
        ~on_cancel:(fun () ->
          if not Sys.win32
          then Process_watcher.kill_process_group pid Term ~is_process_group_leader;
          let sleep = Async_io.sleep t.async_io sigterm_grace_period in
          sigkill_alarm := Some sleep;
          Async_io.Task.await sleep
          >>| function
          | Error `Cancelled -> ()
          | Ok () -> Process_watcher.kill_process_group pid Kill ~is_process_group_leader
          | Error (`Exn _) -> assert false)
        wait
    in
    ( res
    , (match outcome with
       | Cancelled () -> Cancel
       | Not_cancelled -> Normal) )
;;

let got_shutdown reason =
  if !Log.verbose
  then (
    match (reason : Shutdown.Reason.t) with
    | Failure -> Log.info "Shutdown" [ "reason", Dyn.variant "Failure" [] ]
    | Timeout -> Log.info "Shutdown" [ "reason", Dyn.variant "Timeout" [] ]
    | Requested -> Log.info "Shutdown" [ "reason", Dyn.variant "Requested" [] ]
    | Signal signal ->
      Log.info "Shutdown" [ "reason", Dyn.variant "Signal" [ Signal.to_dyn signal ] ])
;;

type saw_shutdown =
  | Ok
  | Got_shutdown

let rec cleanup_iter t saw_shutdown =
  Console.Status_line.refresh ();
  match Event.Queue.next t.events with
  | Job_complete_ready ->
    (match Process_watcher.wait_unix t.process_watcher with
     | [] -> cleanup_iter t saw_shutdown
     | fills -> fills)
  | Fiber_fill_ivar fill -> [ fill ]
  | Shutdown reason ->
    got_shutdown reason;
    saw_shutdown := Got_shutdown;
    cleanup_iter t saw_shutdown
;;

let kill_and_wait_for_all_processes t =
  let saw_shutdown = ref Ok in
  if Sys.win32
  then
    (* SIGTERM is not meaningful on Windows, and [Process_watcher.wait_unix]
       would raise because [Proc.wait Any] is not supported there. Send
       SIGKILL directly; the main drain loop below observes exits via
       [jobs_completed] events pushed by the Win32 polling thread. *)
    Process_watcher.killall t.process_watcher Kill
  else if Process_watcher.running_count t.process_watcher > 0
  then (
    Dune_trace.emit Process Dune_trace.Event.process_cleanup_start;
    (* Send SIGTERM first to give processes a chance to clean up *)
    Process_watcher.killall t.process_watcher Term;
    (* Poll until all processes exit or the grace period expires, then SIGKILL *)
    let deadline = Time.add (Time.now ()) sigterm_grace_period in
    let sent_sigkill = ref false in
    while Process_watcher.running_count t.process_watcher > 0 do
      ignore (Process_watcher.wait_unix t.process_watcher : Fiber.fill list);
      if Process_watcher.running_count t.process_watcher > 0
      then
        if (not !sent_sigkill) && Time.(now () >= deadline)
        then (
          Dune_trace.emit Process Dune_trace.Event.process_cleanup_sigkill;
          Process_watcher.killall t.process_watcher Kill;
          sent_sigkill := true)
        else Unix.sleepf 0.01
    done;
    Dune_trace.emit Process Dune_trace.Event.process_cleanup_finish);
  while Process_watcher.running_count t.process_watcher > 0 do
    ignore (cleanup_iter t saw_shutdown : Fiber.fill list)
  done;
  if Lazy.force child_subreaper_enabled
  then
    cleanup_subreaper_child_processes_impl t
    |> Fiber.run ~iter:(fun () ->
      let rec next () =
        match Event.Queue.next t.events with
        | Fiber_fill_ivar fill -> [ fill ]
        | Job_complete_ready | Shutdown _ -> next ()
      in
      next ());
  (* This silliness is needed because we have tests that run the scheduler
     more than once per process. Such tests require the signal watcher to be
     reset with the correct event queue. *)
  if not Sys.win32
  then (
    Pid.kill_exn (Pid.me ()) `Pid Thread0.signal_watcher_interrupt;
    Thread.join t.signal_watcher);
  !saw_shutdown
;;

let to_dyn { events; process_watcher; _ } =
  Dyn.record
    [ "events", Event.Queue.to_dyn events
    ; "process_watcher", Process_watcher.to_dyn process_watcher
    ]
;;

let () =
  Debug.register ~name:"scheduler" (fun () ->
    match !current with
    | None -> Dyn.Option None
    | Some s -> to_dyn s)
;;

let prepare (config : Config.t) ~events ~file_watcher =
  ignore (Lazy.force child_subreaper_enabled : bool);
  (* The signal watcher must be initialized first so that signals are
     blocked in all threads. *)
  let signal_watcher =
    Signal_watcher.init ~print_ctrl_c_warning:config.print_ctrl_c_warning events
  in
  let process_watcher = Process_watcher.init events in
  let async_io = Async_io.create events in
  let t =
    { job_throttle = Fiber.Throttle.create config.concurrency
    ; process_watcher
    ; events
    ; file_watcher
    ; thread_pool = lazy (Thread_pool.create ~min_workers:4 ~max_workers:50)
    ; signal_watcher
    ; async_io
    ; preserved_child_processes =
        (match
           match file_watcher with
           | None -> None
           | Some file_watcher -> File_watcher.child_pids file_watcher
         with
         | None -> Pid.Set.empty
         | Some p -> Pid.Set.singleton p)
    }
  in
  current := Some t;
  t
;;

let file_watcher () = (t ()).file_watcher

module Run_once = struct
  type run_error =
    | Already_reported
    | Shutdown_requested of Shutdown.Reason.t
    | Exn of Exn_with_backtrace.t

  exception Abort of run_error

  (** This function is the heart of the scheduler. It makes progress in
      executing fibers by doing the following:

      - notifying completed jobs
      - starting cancellations
      - terminating the scheduler on signals *)
  let rec iter (t : t) : Fiber.fill list =
    Console.Status_line.refresh ();
    match Event.Queue.next t.events with
    | Job_complete_ready ->
      (match Process_watcher.wait_unix t.process_watcher with
       | [] -> iter t
       | fills -> fills)
    | Fiber_fill_ivar fill -> [ fill ]
    | Shutdown reason ->
      got_shutdown reason;
      raise @@ Abort (Shutdown_requested reason)
  ;;

  let run t f : _ result =
    let fiber =
      Fiber.map_reduce_errors
        (module Monoid.Unit)
        f
        ~on_error:(fun e ->
          Dune_util.Report_error.report e;
          Fiber.return ())
    in
    match Fiber.run fiber ~iter:(fun () -> iter t) with
    | Ok res ->
      assert (Process_watcher.running_count t.process_watcher = 0);
      Result.Ok res
    | Error () -> Error Already_reported
    | exception Abort err -> Error err
    | exception exn -> Error (Exn (Exn_with_backtrace.capture exn))
  ;;

  let run_and_cleanup t f =
    let res = run t f in
    Option.iter t.file_watcher ~f:File_watcher.shutdown;
    Console.Status_line.clear ();
    let cleanup_result = kill_and_wait_for_all_processes t in
    Async_io.shutdown t.async_io;
    match cleanup_result with
    | Got_shutdown -> Error Already_reported
    | Ok -> res
  ;;
end

let async f =
  let* () = Fiber.return () in
  let t = t () in
  let ivar = Fiber.Ivar.create () in
  let f () =
    let res = Exn_with_backtrace.try_with f in
    Event.Queue.send_worker_tasks_completed t.events [ Fiber.Fill (ivar, res) ]
  in
  Thread_pool.task (Lazy.force t.thread_pool) ~f;
  Fiber.Ivar.read ivar
;;

let async_exn f =
  async f
  >>| function
  | Error exn -> Exn_with_backtrace.reraise exn
  | Ok e -> e
;;

let flush_file_watcher () =
  match (t ()).file_watcher with
  | None -> Fiber.return ()
  | Some file_watcher -> File_watcher.flush file_watcher
;;

module Run = struct
  exception Build_cancelled = Build_cancelled

  module Shutdown = Shutdown

  type file_watcher =
    | Automatic
    | No_watcher

  let file_watcher_equal a b =
    match a, b with
    | Automatic, Automatic | No_watcher, No_watcher -> true
    | _, _ -> false
  ;;

  let go (config : Config.t) ?timeout ?(file_watcher = No_watcher) run =
    let events = Event.Queue.create () in
    let t =
      let file_watcher =
        match file_watcher with
        | No_watcher -> None
        | Automatic ->
          Some
            (File_watcher.create
               ~event_queue:events
               ~watch_exclusions:config.watch_exclusions
               ())
      in
      prepare config ~events ~file_watcher
    in
    match
      let run =
        match timeout with
        | None -> run
        | Some timeout ->
          fun () ->
            let sleep = Async_io.sleep t.async_io timeout in
            Fiber.fork_and_join_unit
              (fun () ->
                 let+ res = Async_io.Task.await sleep in
                 match res with
                 | Ok () -> Event.Queue.send_shutdown t.events Timeout
                 | Error `Cancelled -> ()
                 | Error (`Exn _) -> assert false)
              (fun () ->
                 Fiber.finalize run ~finally:(fun () -> Async_io.Task.cancel sleep))
      in
      match Run_once.run_and_cleanup t run with
      | Ok a -> Result.Ok a
      | Error (Shutdown_requested reason) -> Error (Shutdown.E reason, None)
      | Error Already_reported -> Error (Dune_util.Report_error.Already_reported, None)
      | Error (Exn exn_with_bt) -> Error (exn_with_bt.exn, Some exn_with_bt.backtrace)
    with
    | Ok a -> a
    | Error (exn, None) -> Exn.raise exn
    | Error (exn, Some bt) -> Exn.raise_with_backtrace exn bt
  ;;
end

let shutdown reason =
  let reason =
    match reason with
    | `Ok -> Shutdown.Reason.Requested
    | `Failure -> Shutdown.Reason.Failure
  in
  let t = t () in
  Event.Queue.send_shutdown t.events reason
;;

let wait_for_process_with_timeout t pid waiter ~timeout ~is_process_group_leader =
  Fiber.of_thunk (fun () ->
    let sleep = Async_io.sleep t.async_io timeout in
    let+ clock_result =
      Async_io.Task.await sleep
      >>| function
      | Ok () when Process_watcher.is_running t.process_watcher pid ->
        Process_watcher.kill_process_group pid Kill ~is_process_group_leader;
        Dune_trace.emit Process (fun () ->
          Dune_trace.Event.signal_sent
            Kill
            (`Timeout { pid; group_leader = is_process_group_leader; timeout }));
        `Timed_out
      | Ok () | Error `Cancelled -> `Finished
      | Error (`Exn _) -> assert false
    and+ res, termination_reason =
      let* res = waiter t ~is_process_group_leader pid in
      let+ () = Async_io.Task.cancel sleep in
      res
    in
    ( res
    , match clock_result with
      | `Timed_out -> Timeout
      | `Finished -> termination_reason ))
;;

let wait_for_build_process ?cancellation ?timeout ~is_process_group_leader pid =
  let* () = Fiber.return () in
  let t = t () in
  match timeout with
  | None -> wait_for_build_process t ?cancellation ~is_process_group_leader pid
  | Some timeout ->
    wait_for_process_with_timeout
      t
      pid
      (fun t ~is_process_group_leader pid ->
         wait_for_build_process t ?cancellation ~is_process_group_leader pid)
      ~timeout
      ~is_process_group_leader
;;

let wait_for_process ?timeout ~is_process_group_leader pid =
  wait_for_build_process ?timeout ~is_process_group_leader pid >>| fst
;;

let sleep dur =
  let* () = Fiber.return () in
  (let t = t () in
   Async_io.Task.await (Async_io.sleep t.async_io dur))
  >>| function
  | Ok () -> ()
  | Error `Cancelled ->
    (* cancellation mechanism isn't exposed to the user *)
    assert false
  | Error (`Exn _) -> assert false
;;
