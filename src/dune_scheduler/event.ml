open Stdune

type job =
  { pid : Pid.t
  ; is_process_group_leader : bool
  ; ivar : Proc.Process_info.t Fiber.Ivar.t
  }

let dyn_of_job { pid; is_process_group_leader; ivar } =
  Dyn.record
    [ "pid", Dyn.int (Pid.to_int pid)
    ; "is_process_group_leader", Dyn.bool is_process_group_leader
    ; "ivar", Dyn.opaque ivar
    ]
;;

type build_input_change =
  | Fs_event of File_watcher.Fs_memo_event.t
  | Invalidation of Memo.Invalidation.t

type t =
  | File_watcher_task of (unit -> File_watcher.Event.t list)
  | Build_inputs_changed of build_input_change Nonempty_list.t
  | File_system_sync of File_watcher.Sync_id.t
  | File_system_watcher_terminated
  | Shutdown of Shutdown.Reason.t
  | Fiber_fill_ivar of Fiber.fill
  | Signal_received of Signal.t

module Invalidation_event = struct
  type t =
    | Invalidation of Memo.Invalidation.t
    | Filesystem_event of File_watcher.Event.t
end

module Queue = struct
  type event = t

  module Wakeup = struct
    type t

    external create : unit -> t = "dune_event_wakeup_create"
    external release : t -> unit = "dune_event_wakeup_release"
    external acquire : t -> unit = "dune_event_wakeup_acquire"
    external try_acquire : t -> bool = "dune_event_wakeup_try_acquire"
    external use_for_signals : t -> unit = "dune_event_wakeup_use_for_signals"
    external stop_using_signals : t -> unit = "dune_event_wakeup_stop_using_signals"
  end

  external next_pending_signal_number : unit -> int = "dune_signal_watcher_next_pending"

  external has_pending_signal : unit -> bool = "dune_signal_watcher_has_pending"
  [@@noalloc]

  type t =
    { jobs_completed : (job * Proc.Process_info.t) Queue.t
    ; file_watcher_tasks : (unit -> File_watcher.Event.t list) Queue.t
    ; mutable invalidation_events : Invalidation_event.t list
    ; mutable shutdown_reasons : Shutdown.Reason.Set.t
    ; mutex : Mutex.t
    ; wakeup : Wakeup.t
    ; mutable pending_jobs : int
    ; mutable pending_worker_tasks : int
    ; worker_tasks_completed : Fiber.fill Queue.t
    ; mutable got_event : bool
    ; mutable yield : unit Fiber.Ivar.t option
    }

  let to_dyn { pending_jobs; pending_worker_tasks; _ } =
    Dyn.record
      [ "pending_jobs", Dyn.int pending_jobs
      ; "pending_worker_tasks", Dyn.int pending_worker_tasks
      ]
  ;;

  let create () =
    let jobs_completed = Queue.create () in
    let file_watcher_tasks = Queue.create () in
    let worker_tasks_completed = Queue.create () in
    let invalidation_events = [] in
    let shutdown_reasons = Shutdown.Reason.Set.empty in
    let mutex = Mutex.create () in
    let wakeup = Wakeup.create () in
    let pending_jobs = 0 in
    let pending_worker_tasks = 0 in
    { jobs_completed
    ; file_watcher_tasks
    ; invalidation_events
    ; shutdown_reasons
    ; mutex
    ; wakeup
    ; pending_jobs
    ; worker_tasks_completed
    ; pending_worker_tasks
    ; got_event = false
    ; yield = None
    }
  ;;

  let register_job_started q = q.pending_jobs <- q.pending_jobs + 1

  let finish_job q =
    assert (q.pending_jobs > 0);
    q.pending_jobs <- q.pending_jobs - 1
  ;;

  let register_worker_task_started q =
    q.pending_worker_tasks <- q.pending_worker_tasks + 1
  ;;

  let cancel_work_task_started q = q.pending_worker_tasks <- q.pending_worker_tasks - 1

  let add_event q f =
    Mutex.protect q.mutex (fun () ->
      f q;
      if not q.got_event
      then (
        q.got_event <- true;
        Wakeup.release q.wakeup))
  ;;

  let use_for_signal_wakeup q = Wakeup.use_for_signals q.wakeup
  let stop_using_signal_wakeup q = Wakeup.stop_using_signals q.wakeup

  let yield_if_there_are_pending_events q =
    if Execution_env.inside_dune || not (q.got_event || has_pending_signal ())
    then Fiber.return ()
    else (
      match q.yield with
      | Some ivar -> Fiber.Ivar.read ivar
      | None ->
        let ivar = Fiber.Ivar.create () in
        q.yield <- Some ivar;
        Fiber.Ivar.read ivar)
  ;;

  module Event_source = struct
    type queue = t
    type t = queue -> event option

    let run t q = t q

    let shutdown : t =
      fun q ->
      Option.map (Shutdown.Reason.Set.choose q.shutdown_reasons) ~f:(fun reason ->
        q.shutdown_reasons <- Shutdown.Reason.Set.remove q.shutdown_reasons reason;
        Shutdown reason)
    ;;

    let signal : t =
      fun _ ->
      match next_pending_signal_number () with
      | 0 -> None
      | signal -> Some (Signal_received (Signal.of_int signal))
    ;;

    let file_watcher_task q =
      Option.map (Queue.pop q.file_watcher_tasks) ~f:(fun job -> File_watcher_task job)
    ;;

    let invalidation q =
      match q.invalidation_events with
      | [] -> None
      | events ->
        let rec process_events acc = function
          | [] ->
            q.invalidation_events <- [];
            Option.map
              (Nonempty_list.of_list (List.rev acc))
              ~f:(fun build_input_changes -> Build_inputs_changed build_input_changes)
          | event :: events ->
            (match (event : Invalidation_event.t) with
             | Filesystem_event Watcher_terminated ->
               q.invalidation_events <- [];
               Some File_system_watcher_terminated
             | Filesystem_event (Sync id) ->
               (match Nonempty_list.of_list (List.rev acc) with
                | None ->
                  q.invalidation_events <- events;
                  Some (File_system_sync id)
                | Some build_input_changes ->
                  q.invalidation_events <- event :: events;
                  Some (Build_inputs_changed build_input_changes))
             | Filesystem_event (Fs_memo_event event) ->
               process_events (Fs_event event :: acc) events
             | Filesystem_event Queue_overflow ->
               process_events
                 (Invalidation
                    (Memo.Invalidation.clear_caches ~reason:Event_queue_overflow)
                  :: acc)
                 events
             | Invalidation invalidation ->
               process_events (Invalidation invalidation :: acc) events)
        in
        process_events [] events
    ;;

    let jobs_completed q =
      Option.map (Queue.pop q.jobs_completed) ~f:(fun (job, proc_info) ->
        q.pending_jobs <- q.pending_jobs - 1;
        assert (q.pending_jobs >= 0);
        Fiber_fill_ivar (Fill (job.ivar, proc_info)))
    ;;

    let worker_tasks_completed q =
      Option.map (Queue.pop q.worker_tasks_completed) ~f:(fun fill ->
        q.pending_worker_tasks <- q.pending_worker_tasks - 1;
        Fiber_fill_ivar fill)
    ;;

    let yield q =
      Option.map q.yield ~f:(fun ivar ->
        q.yield <- None;
        Fiber_fill_ivar (Fill (ivar, ())))
    ;;

    let chain list q = List.find_map list ~f:(fun f -> f q)
  end

  let events_in_order =
    (* Event sources are listed in priority order. Signals are the
       highest priority to maximize responsiveness to Ctrl+C.
       [file_watcher_task], [worker_tasks_completed] and [invalidation] are
       used for reacting to user input, so their latency is also important.
       [jobs_completed] and [yield] are where the bulk of the work is done, so
       they are the lowest priority to avoid starving other things. *)
    Event_source.
      [ signal
      ; shutdown
      ; file_watcher_task
      ; invalidation
      ; worker_tasks_completed
      ; jobs_completed
      ; yield
      ]
  ;;

  let next q =
    Dune_trace.emit ~buffered:true Gc (fun () -> Dune_trace.Event.gc ());
    Dune_trace.emit_all ~buffered:true Fd (fun () ->
      match Dune_trace.Event.fd_count () with
      | None -> []
      | Some fd -> [ fd ]);
    Mutex.lock q.mutex;
    let rec loop () =
      match Event_source.(run (chain events_in_order)) q with
      | None -> wait ()
      | Some event -> event
    and wait () =
      (* Keep a single pending wakeup for any number of events. The token can
         be stale when the scheduler drained events without blocking, so consume
         it before waiting for the next event. *)
      q.got_event <- false;
      ignore (Wakeup.try_acquire q.wakeup : bool);
      match Event_source.(run (chain events_in_order)) q with
      | Some event -> event
      | None ->
        Mutex.unlock q.mutex;
        Wakeup.acquire q.wakeup;
        Mutex.lock q.mutex;
        loop ()
    in
    let ev = loop () in
    Mutex.unlock q.mutex;
    ev
  ;;

  let send_worker_task_completed q event =
    add_event q (fun q -> Queue.push q.worker_tasks_completed event)
  ;;

  let send_worker_tasks_completed q events =
    match events with
    | [] -> ()
    | _ :: _ ->
      add_event q (fun q -> List.iter events ~f:(Queue.push q.worker_tasks_completed))
  ;;

  let send_invalidation_events q events =
    add_event q (fun q -> q.invalidation_events <- q.invalidation_events @ events)
  ;;

  let send_file_watcher_events q files =
    send_invalidation_events
      q
      (List.map files ~f:(fun file : Invalidation_event.t -> Filesystem_event file))
  ;;

  let send_invalidation_event q invalidation =
    send_invalidation_events q [ Invalidation invalidation ]
  ;;

  let send_job_completed q job proc_info =
    add_event q (fun q -> Queue.push q.jobs_completed (job, proc_info))
  ;;

  let send_shutdown q signal =
    add_event q (fun q ->
      q.shutdown_reasons <- Shutdown.Reason.Set.add q.shutdown_reasons signal)
  ;;

  let send_file_watcher_task q job =
    add_event q (fun q -> Queue.push q.file_watcher_tasks job)
  ;;

  let pending_jobs q = q.pending_jobs
  let pending_worker_tasks q = q.pending_worker_tasks

  let%expect_test "wakeup is coalesced while an event is pending" =
    let q = create () in
    let print_wakeup label =
      Printf.printf "%s: %b\n" label (Wakeup.try_acquire q.wakeup)
    in
    let print_next () =
      match next q with
      | Shutdown Requested -> print_endline "requested"
      | Shutdown Timeout -> print_endline "timeout"
      | _ -> print_endline "unexpected"
    in
    print_wakeup "initial";
    send_shutdown q Requested;
    send_shutdown q Timeout;
    print_wakeup "after two events";
    print_wakeup "after consuming token";
    print_next ();
    print_next ();
    print_wakeup "after draining events";
    [%expect
      {|
      initial: false
      after two events: true
      after consuming token: false
      requested
      timeout
      after draining events: false
      |}]
  ;;
end
