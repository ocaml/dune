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

module Fs_memo_event = struct
  type t =
    { path : Path.t
    ; kind : Dune_trace.File_watcher_event.kind
    }

  let to_dyn { path; kind } =
    let open Dyn in
    record
      [ "path", Path.to_dyn path
      ; "kind", Repr.to_dyn Dune_trace.File_watcher_event.kind_repr kind
      ]
  ;;

  let create ~kind ~path = { path; kind }
end

module File_watcher_event = struct
  type t =
    | Fs_memo_event of Fs_memo_event.t
    | Queue_overflow
end

type t =
  | Shutdown of Shutdown.Reason.t
  | Fiber_fill_ivar of Fiber.fill
  | Job_complete_ready

module Queue = struct
  type event = t

  type t =
    { jobs_completed : (job * Proc.Process_info.t) Queue.t
    ; mutable shutdown_reasons : Shutdown.Reason.Set.t
    ; mutex : Mutex.t
    ; cond : Condition.t
    ; mutable job_complete_ready : bool
    ; worker_tasks_completed : Fiber.fill Queue.t
    ; mutable got_event : bool
    ; mutable yield : unit Fiber.Ivar.t option
    }

  let to_dyn _ = Dyn.record []

  let create () =
    let jobs_completed = Queue.create () in
    let worker_tasks_completed = Queue.create () in
    let shutdown_reasons = Shutdown.Reason.Set.empty in
    let mutex = Mutex.create () in
    let cond = Condition.create () in
    { jobs_completed
    ; shutdown_reasons
    ; mutex
    ; cond
    ; worker_tasks_completed
    ; got_event = false
    ; yield = None
    ; job_complete_ready = false
    }
  ;;

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

    let jobs_completed q =
      Option.map (Queue.pop q.jobs_completed) ~f:(fun (job, proc_info) ->
        Fiber_fill_ivar (Fill (job.ivar, proc_info)))
    ;;

    let worker_tasks_completed q =
      Option.map (Queue.pop q.worker_tasks_completed) ~f:(fun fill ->
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
       [worker_tasks_completed] is used for reacting to user input, so its
       latency is also important. [jobs_completed] and [yield] are where the
       bulk of the work is done, so they are the lowest priority to avoid
       starving other things. *)
    Event_source.
      [ shutdown
      ; worker_tasks_completed
      ; (if Sys.win32 then jobs_completed else job_complete_ready)
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
      q.got_event <- false;
      Condition.wait q.cond q.mutex;
      loop ()
    in
    let ev = loop () in
    Mutex.unlock q.mutex;
    ev
  ;;

  let send_worker_tasks_completed q events =
    match events with
    | [] -> ()
    | _ :: _ ->
      add_event q (fun q -> List.iter events ~f:(Queue.push q.worker_tasks_completed))
  ;;

  let send_job_completed q job proc_info =
    add_event q (fun q -> Queue.push q.jobs_completed (job, proc_info))
  ;;

  let send_job_completed_ready q = add_event q (fun q -> q.job_complete_ready <- true)

  let send_shutdown q signal =
    add_event q (fun q ->
      q.shutdown_reasons <- Shutdown.Reason.Set.add q.shutdown_reasons signal)
  ;;
end
