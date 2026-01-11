open Stdune

type job =
  { pid : Pid.t
  ; ivar : Proc.Process_info.t Fiber.Ivar.t
  }

type build_input_change =
  | Fs_event of Dune_file_watcher.Fs_memo_event.t
  | Invalidation of Memo.Invalidation.t

type t =
  | File_watcher_task of (unit -> Dune_file_watcher.Event.t list)
  | Build_inputs_changed of build_input_change Nonempty_list.t
  | File_system_sync of Dune_file_watcher.Sync_id.t
  | File_system_watcher_terminated
  | Shutdown of Shutdown.Reason.t
  | Fiber_fill_ivar of Fiber.fill
  | Job_complete_ready

module Invalidation_event = struct
  type t =
    | Invalidation of Memo.Invalidation.t
    | Filesystem_event of Dune_file_watcher.Event.t
end

module Queue = struct
  type event = t

  type t =
    { jobs_completed : (job * Proc.Process_info.t) Queue.t
    ; file_watcher_tasks : (unit -> Dune_file_watcher.Event.t list) Queue.t
    ; mutable invalidation_events : Invalidation_event.t list
    ; mutable shutdown_reasons : Shutdown.Reason.Set.t
    ; mutex : Mutex.t
    ; cond : Condition.t
    ; mutable pending_jobs : int
    ; mutable pending_worker_tasks : int
    ; mutable job_complete_ready : bool
    ; worker_tasks_completed : Fiber.fill Queue.t
    ; timers : Fiber.fill Queue.t
    ; mutable got_event : bool
    ; mutable yield : unit Fiber.Ivar.t option
    }

  let create () =
    let jobs_completed = Queue.create () in
    let file_watcher_tasks = Queue.create () in
    let worker_tasks_completed = Queue.create () in
    let invalidation_events = [] in
    let shutdown_reasons = Shutdown.Reason.Set.empty in
    let mutex = Mutex.create () in
    let cond = Condition.create () in
    let pending_jobs = 0 in
    let pending_worker_tasks = 0 in
    let timers = Queue.create () in
    { jobs_completed
    ; file_watcher_tasks
    ; invalidation_events
    ; timers
    ; shutdown_reasons
    ; mutex
    ; cond
    ; pending_jobs
    ; worker_tasks_completed
    ; pending_worker_tasks
    ; got_event = false
    ; yield = None
    ; job_complete_ready = false
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
    Mutex.lock q.mutex;
    f q;
    if not q.got_event
    then (
      q.got_event <- true;
      Condition.signal q.cond);
    Mutex.unlock q.mutex
  ;;

  let yield_if_there_are_pending_events q =
    if Execution_env.inside_dune || not q.got_event
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

    let job_complete_ready : t =
      fun q ->
      match q.job_complete_ready with
      | false -> None
      | true ->
        q.job_complete_ready <- false;
        Some Job_complete_ready
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

    let timers q = Option.map (Queue.pop q.timers) ~f:(fun timer -> Fiber_fill_ivar timer)
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
      [ shutdown
      ; file_watcher_task
      ; invalidation
      ; worker_tasks_completed
      ; (if Sys.win32 then jobs_completed else job_complete_ready)
      ; yield
      ; timers
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
      q.got_event <- false;
      Condition.wait q.cond q.mutex;
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

  let send_job_completed_ready q = add_event q (fun q -> q.job_complete_ready <- true)

  let send_shutdown q signal =
    add_event q (fun q ->
      q.shutdown_reasons <- Shutdown.Reason.Set.add q.shutdown_reasons signal)
  ;;

  let send_file_watcher_task q job =
    add_event q (fun q -> Queue.push q.file_watcher_tasks job)
  ;;

  let send_timers_completed q timers =
    add_event q (fun q ->
      Nonempty_list.to_list timers |> List.iter ~f:(Queue.push q.timers))
  ;;

  let pending_jobs q = q.pending_jobs
  let pending_worker_tasks q = q.pending_worker_tasks
end
