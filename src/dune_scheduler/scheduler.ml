open Import
open Fiber.O

module Fs_memo = struct
  let handle_fs_event = Fdecl.create Dyn.opaque
  let init = Fdecl.create Dyn.opaque

  let set_impl ~handle_fs_event:handle_fs_event' ~init:init' =
    Fdecl.set handle_fs_event handle_fs_event';
    Fdecl.set init init'
  ;;

  let handle_fs_event event = Fdecl.get handle_fs_event event
  let init ~dune_file_watcher = Fdecl.get init ~dune_file_watcher
end

module Config = struct
  type t =
    { concurrency : int
    ; print_ctrl_c_warning : bool
    ; watch_exclusions : string list
    }
end

let spawn_thread ~name f = Thread0.spawn ~name f

module Run_id = Run_id
include Types.Scheduler

let running_jobs_count (t : t) = Event.Queue.pending_jobs t.events

exception Build_cancelled

let cancelled () = raise (Memo.Non_reproducible Build_cancelled)
let check_cancelled t = if Fiber.Cancel.fired t.cancel then cancelled ()

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

let with_job_slot f =
  let* () = Fiber.return () in
  let t = t () in
  Fiber.Throttle.run t.job_throttle ~f:(fun () ->
    check_cancelled t;
    f ())
;;

let wait_for_process t ~is_process_group_leader pid =
  let ivar = Fiber.Ivar.create () in
  Process_watcher.register_job t.process_watcher { pid; is_process_group_leader; ivar };
  Fiber.Ivar.read ivar
;;

(* Grace period before escalating from SIGTERM to SIGKILL *)
let sigterm_grace_period = Time.Span.of_secs 0.2

type termination_reason =
  | Normal
  | Cancel
  | Timeout

(* We use this version privately in this module whenever we can pass the
   scheduler explicitly *)
let wait_for_build_process t ~is_process_group_leader pid =
  let sigkill_alarm = ref None in
  let+ res, outcome =
    Fiber.Cancel.with_handler
      t.cancel
      ~on_cancel:(fun () ->
        if not Sys.win32 then Process_watcher.killall t.process_watcher Sys.sigterm;
        let sleep = Async_io.sleep t.async_io sigterm_grace_period in
        sigkill_alarm := Some sleep;
        Async_io.Task.await sleep
        >>| function
        | Error `Cancelled -> ()
        | Ok () -> Process_watcher.killall t.process_watcher Sys.sigkill
        | Error (`Exn _) -> assert false)
      (fun () ->
         let* r = wait_for_process t ~is_process_group_leader pid in
         if not Sys.win32
         then Process_watcher.kill_process_group pid Sys.sigterm ~is_process_group_leader;
         let+ () =
           match !sigkill_alarm with
           | None -> Fiber.return ()
           | Some alarm -> Async_io.Task.cancel alarm
         in
         r)
  in
  ( res
  , match outcome with
    | Cancelled () -> Cancel
    | Not_cancelled -> Normal )
;;

let got_shutdown reason =
  if !Log.verbose
  then (
    match (reason : Shutdown.Reason.t) with
    | Timeout -> Log.info "Shutdown" [ "reason", Dyn.variant "Timeout" [] ]
    | Requested -> Log.info "Shutdown" [ "reason", Dyn.variant "Requested" [] ]
    | Signal signal ->
      Log.info "Shutdown" [ "reason", Dyn.variant "Signal" [ Signal.to_dyn signal ] ])
;;

let filesystem_watcher_terminated () =
  Log.info "Shutdown" [ "reason", Dyn.string "Filesystem watcher terminated" ]
;;

type saw_shutdown =
  | Ok
  | Got_shutdown

let kill_and_wait_for_all_processes t =
  if Sys.win32
  then
    (* SIGTERM is not meaningful on Windows, and [Process_watcher.wait_unix]
       would raise because [Proc.wait] has no implementation there. Send
       SIGKILL directly; the main drain loop below observes exits via
       [jobs_completed] events pushed by the Win32 polling thread. *)
    Process_watcher.killall t.process_watcher Sys.sigkill
  else if Event.Queue.pending_jobs t.events > 0
  then (
    Dune_trace.emit Process Dune_trace.Event.process_cleanup_start;
    (* Send SIGTERM first to give processes a chance to clean up *)
    Process_watcher.killall t.process_watcher Sys.sigterm;
    (* Poll until all processes exit or the grace period expires, then SIGKILL *)
    let deadline = Time.add (Time.now ()) sigterm_grace_period in
    let sent_sigkill = ref false in
    while Event.Queue.pending_jobs t.events > 0 do
      ignore (Process_watcher.wait_unix t.process_watcher : Fiber.fill list);
      if Event.Queue.pending_jobs t.events > 0
      then
        if (not !sent_sigkill) && Time.(now () >= deadline)
        then (
          Dune_trace.emit Process Dune_trace.Event.process_cleanup_sigkill;
          Process_watcher.killall t.process_watcher Sys.sigkill;
          sent_sigkill := true)
        else Unix.sleepf 0.01
    done;
    Dune_trace.emit Process Dune_trace.Event.process_cleanup_finish);
  let saw_signal = ref Ok in
  while Event.Queue.pending_jobs t.events > 0 do
    match Event.Queue.next t.events with
    | Shutdown reason ->
      got_shutdown reason;
      saw_signal := Got_shutdown
    | Job_complete_ready ->
      ignore (Process_watcher.wait_unix t.process_watcher : Fiber.fill list)
    | _ -> ()
  done;
  (* This silliness is needed because we have tests that run the scheduler
     more than once per process. Such tests require the signal watcher to be
     reset with the correct event queue. *)
  if not Sys.win32
  then (
    Unix.kill (Unix.getpid ()) (Signal.to_int Thread0.signal_watcher_interrupt);
    Thread.join t.signal_watcher);
  !saw_signal
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

let prepare (config : Config.t) ~(handler : Handler.t) ~events ~file_watcher =
  (* The signal watcher must be initialized first so that signals are
     blocked in all threads. *)
  let signal_watcher =
    Signal_watcher.init ~print_ctrl_c_warning:config.print_ctrl_c_warning events
  in
  let cancel = Fiber.Cancel.create () in
  let process_watcher = Process_watcher.init events in
  let async_io = Async_io.create events in
  let t =
    { status =
        (* Slightly weird initialization happening here: for polling mode we
           initialize in "Building" state, immediately switch to Standing_by
           and then back to "Building". It would make more sense to start in
           "Stand_by" from the start. We can't "just" switch the initial value
           here because then the non-polling mode would run in "Standing_by"
           mode, which is even weirder. *)
        Building cancel
    ; invalidation = Memo.Invalidation.empty
    ; run_id_state = Run_id.State.create ~watch_mode:(Option.is_some file_watcher)
    ; watch_restart_started_at = None
    ; job_throttle = Fiber.Throttle.create config.concurrency
    ; process_watcher
    ; events
    ; handler
    ; file_watcher
    ; fs_syncs = Table.create (module File_watcher.Sync_id) 64
    ; build_inputs_changed = Trigger.create ()
    ; cancel
    ; thread_pool = lazy (Thread_pool.create ~min_workers:4 ~max_workers:50)
    ; signal_watcher
    ; async_io
    }
  in
  current := Some t;
  t
;;

module Run_once = struct
  type run_error =
    | Already_reported
    | Shutdown_requested of Shutdown.Reason.t
    | Exn of Exn_with_backtrace.t

  exception Abort of run_error

  let handle_invalidation_events =
    let handle_event event =
      match (event : Event.build_input_change) with
      | Invalidation invalidation -> invalidation
      | Fs_event event -> Fs_memo.handle_fs_event event
    in
    fun events ->
      let events = Nonempty_list.to_list events in
      List.fold_left events ~init:Memo.Invalidation.empty ~f:(fun acc event ->
        Memo.Invalidation.combine acc (handle_event event))
  ;;

  (** This function is the heart of the scheduler. It makes progress in
      executing fibers by doing the following:

      - notifying completed jobs
      - starting cancellations
      - terminating the scheduler on signals *)
  let rec iter (t : t) : Fiber.fill list =
    t.handler Tick;
    match Event.Queue.next t.events with
    | File_watcher_task job ->
      let events = job () in
      Event.Queue.send_file_watcher_events t.events events;
      iter t
    | File_system_sync id ->
      (match Table.find t.fs_syncs id with
       | None -> iter t
       | Some ivar ->
         Table.remove t.fs_syncs id;
         [ Fill (ivar, ()) ])
    | Build_inputs_changed events -> build_input_change t events
    | File_system_watcher_terminated ->
      filesystem_watcher_terminated ();
      raise (Abort Already_reported)
    | Job_complete_ready ->
      (match Process_watcher.wait_unix t.process_watcher with
       | [] -> iter t
       | fills -> fills)
    | Fiber_fill_ivar fill -> [ fill ]
    | Shutdown reason ->
      got_shutdown reason;
      raise @@ Abort (Shutdown_requested reason)

  and build_input_change (t : t) events =
    let invalidation = handle_invalidation_events events in
    if Memo.Invalidation.is_empty invalidation
    then iter t
    else (
      let now = Time.now () in
      let reasons = Memo.Invalidation.details_hum ~max_elements:max_int invalidation in
      if Run_id.State.is_watch t.run_id_state
      then (
        let run_id = Run_id.State.next_to_start t.run_id_state in
        if Option.is_none t.watch_restart_started_at
        then t.watch_restart_started_at <- Some now;
        Dune_trace.emit Build (fun () ->
          Dune_trace.Event.watch_build_restart
            ~run_id:(Run_id.to_int run_id)
            ~reasons
            ~at:now));
      t.invalidation <- Memo.Invalidation.combine t.invalidation invalidation;
      let fills =
        match t.status with
        | Restarting_build | Standing_by -> []
        | Building cancellation ->
          t.handler Build_interrupted;
          t.status <- Restarting_build;
          Fiber.Cancel.fire' cancellation
      in
      let fills = Trigger.trigger t.build_inputs_changed @ fills in
      match fills with
      | [] -> iter t
      | fills -> fills)
  ;;

  let run t f : _ result =
    current := Some t;
    let fiber =
      Fiber.map_reduce_errors
        (module Monoid.Unit)
        f
        ~on_error:(fun e ->
          Dune_util.Report_error.report e;
          Fiber.return ())
    in
    Exn.protect
      ~finally:(fun () -> current := None)
      ~f:(fun () ->
        match Fiber.run fiber ~iter:(fun () -> iter t) with
        | Ok res ->
          assert (Event.Queue.pending_jobs t.events = 0);
          assert (Event.Queue.pending_worker_tasks t.events = 0);
          Result.Ok res
        | Error () -> Error Already_reported
        | exception Abort err -> Error err
        | exception exn -> Error (Exn (Exn_with_backtrace.capture exn)))
  ;;

  let run_and_cleanup t f =
    let res = run t f in
    Async_io.shutdown t.async_io;
    Option.iter t.file_watcher ~f:(fun watcher ->
      (* CR-someday rgrinberg: we do not wind down the threads for the file
         watchers currently. Might interefere with tests that spawn the
         scheduler more than once *)
      match File_watcher.shutdown watcher with
      | `Kill pid ->
        (* CR-someday rgrinberg: Instead of this hackery, we should probably
           just rgister the watcher as a non build process. Luckily, this code
           path is incredibly rare as the external file watchers are bad. *)
        Unix.kill (Pid.to_int pid) Sys.sigterm
      | `Thunk f -> f ()
      | `No_op -> ());
    Console.Status_line.clear ();
    match kill_and_wait_for_all_processes t with
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
    Event.Queue.send_worker_task_completed t.events (Fiber.Fill (ivar, res))
  in
  Thread_pool.task (Lazy.force t.thread_pool) ~f;
  Event.Queue.register_worker_task_started t.events;
  Fiber.Ivar.read ivar
;;

let async_exn f =
  async f
  >>| function
  | Error exn -> Exn_with_backtrace.reraise exn
  | Ok e -> e
;;

let flush_file_watcher t =
  match t.file_watcher with
  | None -> Fiber.return ()
  | Some file_watcher ->
    let ivar = Fiber.Ivar.create () in
    let id = File_watcher.emit_sync file_watcher in
    Table.set t.fs_syncs id ivar;
    Fiber.Ivar.read ivar
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

  module Build_outcome = Build_outcome
  module Event_queue = Event.Queue
  module Event = Handler.Event

  let allocate_run_id t =
    let state, run_id = Run_id.State.start t.run_id_state in
    t.run_id_state <- state;
    run_id
  ;;

  let emit_build_start ~run_id ~restart ~files ~start =
    Dune_trace.emit ~buffered:true Build (fun () ->
      Dune_trace.Event.watch_build_start
        ~run_id:(Run_id.to_int run_id)
        ~restart
        ~files
        ~start)
  ;;

  let emit_build_finish ~run_id ~start ~stop ~outcome ~restart_duration =
    Dune_trace.emit ~buffered:true Build (fun () ->
      Dune_trace.Event.watch_build_finish
        ~run_id:(Run_id.to_int run_id)
        ~outcome
        ~start
        ~stop
        ~restart_duration)
  ;;

  let rec poll_iter t step =
    let files =
      if Memo.Invalidation.is_empty t.invalidation
      then (
        Memo.Metrics.reset ();
        None)
      else (
        let files = Memo.Invalidation.changed_paths t.invalidation in
        let details_hum = Memo.Invalidation.details_hum t.invalidation in
        t.handler (Source_files_changed { details_hum });
        Memo.reset t.invalidation;
        t.invalidation <- Memo.Invalidation.empty;
        t.build_inputs_changed <- Trigger.create ();
        Some files)
    in
    let started_at = Time.now () in
    let run_id = allocate_run_id t in
    emit_build_start
      ~run_id
      ~restart:(Option.is_some t.watch_restart_started_at)
      ~files
      ~start:started_at;
    let cancel = Fiber.Cancel.create () in
    t.status <- Building cancel;
    t.cancel <- cancel;
    let* res = step in
    match t.status with
    | Standing_by ->
      let res : Build_outcome.t =
        match res with
        | Error `Already_reported -> Failure
        | Ok () -> Success
      in
      let stop = Time.now () in
      let restart_duration =
        Option.map t.watch_restart_started_at ~f:(fun restart_started_at ->
          Time.diff stop restart_started_at)
      in
      t.watch_restart_started_at <- None;
      emit_build_finish
        ~run_id
        ~start:started_at
        ~stop
        ~outcome:
          (match res with
           | Success -> `Success
           | Failure -> `Failure)
        ~restart_duration;
      t.handler (Build_finish res);
      Fiber.return res
    | Restarting_build -> poll_iter t step
    | Building _ ->
      let res : Build_outcome.t =
        match res with
        | Error `Already_reported -> Failure
        | Ok () -> Success
      in
      t.status <- Standing_by;
      let stop = Time.now () in
      let restart_duration =
        Option.map t.watch_restart_started_at ~f:(fun restart_started_at ->
          Time.diff stop restart_started_at)
      in
      t.watch_restart_started_at <- None;
      emit_build_finish
        ~run_id
        ~start:started_at
        ~stop
        ~outcome:
          (match res with
           | Success -> `Success
           | Failure -> `Failure)
        ~restart_duration;
      t.handler (Build_finish res);
      Fiber.return res
  ;;

  let poll_iter t step =
    match t.status with
    | Building _ | Restarting_build -> assert false
    | Standing_by -> poll_iter t step
  ;;

  type step = (unit, [ `Already_reported ]) Result.t Fiber.t

  let poll_init () =
    let+ () = Fiber.return () in
    let t = t () in
    assert (
      match t.status with
      | Building _ -> true
      | _ -> false);
    t.status <- Standing_by;
    t
  ;;

  (* Work we're allowed to do between successive polling iterations. this work
     should be fast and never fail (within reason) *)
  let run_when_idle () : unit =
    Dune_trace.emit ~buffered:true Scheduler Dune_trace.Event.scheduler_idle;
    Dune_trace.flush ()
  ;;

  let poll step =
    let* t = poll_init () in
    let rec loop () =
      let* _res = poll_iter t step in
      run_when_idle ();
      let* () = Trigger.wait t.build_inputs_changed in
      loop ()
    in
    loop ()
  ;;

  let poll_passive ~get_build_request =
    let* t = poll_init () in
    let rec loop () =
      let* step, response_ivar = get_build_request in
      (* Flush before to make the build reproducible. The passive watch mode is
         designed for tests and We want to observe all the change made by the
         test before starting the build. *)
      let* () = flush_file_watcher t in
      let step =
        let* res = step in
        (* Flush after the build to make sure we reach a fix point if the build
           interrupts itself. Without that, a file change caused by the build
           itself might be picked up before or after the build finishes, which
           makes the behavior racy and not good for tests.

           Flushing here makes the previous [flush_file_watcher] less useful,
           however we keep it because without it we might start the build
           without having observed all the changes made by the current test.
           Such an intermediate state might result in a build error, which would
           make the test racy.*)
        let+ () = flush_file_watcher t in
        res
      in
      let* res = poll_iter t step in
      let* () = Fiber.Ivar.fill response_ivar res in
      loop ()
    in
    loop ()
  ;;

  let go
        (config : Config.t)
        ?timeout
        ?(file_watcher = No_watcher)
        ~(on_event : Handler.Event.t -> unit)
        run
    =
    let events = Event_queue.create () in
    let file_watcher =
      match file_watcher with
      | No_watcher -> None
      | Automatic ->
        Some
          (File_watcher.create_default
             ~scheduler:
               { thread_safe_send_emit_events_job =
                   (fun job -> Event_queue.send_file_watcher_task events job)
               }
             ~watch_exclusions:config.watch_exclusions
             ())
    in
    let t = prepare config ~handler:on_event ~events ~file_watcher in
    Option.iter file_watcher ~f:(fun dune_file_watcher ->
      let initial_invalidation =
        Fs_memo.init ~dune_file_watcher:(Some dune_file_watcher)
      in
      Memo.reset initial_invalidation);
    let result =
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
                 | Ok () -> Event_queue.send_shutdown t.events Timeout
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
    in
    match result with
    | Ok a -> a
    | Error (exn, None) -> Exn.raise exn
    | Error (exn, Some bt) -> Exn.raise_with_backtrace exn bt
  ;;
end

let shutdown () =
  let t = t () in
  Event.Queue.send_shutdown t.events Requested
;;

let cancel_current_build () =
  let* () = Fiber.return () in
  let t = t () in
  match t.status with
  | Restarting_build | Standing_by -> Fiber.return ()
  | Building cancellation ->
    t.handler Build_interrupted;
    t.status <- Standing_by;
    Fiber.Cancel.fire cancellation
;;

let wait_for_process_with_timeout t pid waiter ~timeout ~is_process_group_leader =
  Fiber.of_thunk (fun () ->
    let sleep = Async_io.sleep t.async_io timeout in
    let+ clock_result =
      Async_io.Task.await sleep
      >>| function
      | Ok () when Process_watcher.is_running t.process_watcher pid ->
        Process_watcher.kill_process_group pid Sys.sigkill ~is_process_group_leader;
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

let wait_for_build_process ?timeout ~is_process_group_leader pid =
  let* () = Fiber.return () in
  let t = t () in
  match timeout with
  | None -> wait_for_build_process t ~is_process_group_leader pid
  | Some timeout ->
    wait_for_process_with_timeout
      t
      pid
      wait_for_build_process
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

let set_fs_memo_impl = Fs_memo.set_impl

module For_tests = struct
  let wait_for_build_input_change () =
    let* () = Fiber.return () in
    let t = t () in
    Trigger.wait t.build_inputs_changed
  ;;

  let inject_memo_invalidation invalidation =
    let* () = Fiber.return () in
    let t = t () in
    Event.Queue.send_invalidation_event t.events invalidation;
    Fiber.return ()
  ;;
end
