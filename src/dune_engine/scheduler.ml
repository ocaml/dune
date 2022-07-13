open Import
open Fiber.O

module Config = struct
  include Config

  module Display = struct
    type verbosity =
      | Quiet
      | Short
      | Verbose

    type t =
      { status_line : bool
      ; verbosity : verbosity
      }

    (* Even though [status_line] is true by default in most of these, the status
       line is actually not shown if the output is redirected to a file or a
       pipe. *)
    let all =
      [ ("progress", { verbosity = Quiet; status_line = true })
      ; ("verbose", { verbosity = Verbose; status_line = true })
      ; ("short", { verbosity = Short; status_line = true })
      ; ("quiet", { verbosity = Quiet; status_line = false })
      ]

    let verbosity_to_dyn : verbosity -> Dyn.t = function
      | Quiet -> Variant ("Quiet", [])
      | Short -> Variant ("Short", [])
      | Verbose -> Variant ("Verbose", [])

    let to_dyn { status_line; verbosity } : Dyn.t =
      Record
        [ ("status_line", Dyn.Bool status_line)
        ; ("verbosity", verbosity_to_dyn verbosity)
        ]

    let console_backend t =
      match t.status_line with
      | false -> Console.Backend.dumb
      | true -> Console.Backend.progress
  end

  type t =
    { concurrency : int
    ; display : Display.t
    ; stats : Dune_stats.t option
    ; insignificant_changes : [ `Ignore | `React ]
    }
end

type job =
  { pid : Pid.t
  ; ivar : Proc.Process_info.t Fiber.Ivar.t
  }

module Signal = struct
  type t =
    | Int
    | Quit
    | Term

  let to_dyn t : Dyn.t =
    match t with
    | Int -> Variant ("Int", [])
    | Quit -> Variant ("Quit", [])
    | Term -> Variant ("Term", [])

  let compare x y =
    match (x, y) with
    | Int, Int -> Eq
    | Int, _ -> Lt
    | _, Int -> Gt
    | Quit, Quit -> Eq
    | Quit, _ -> Lt
    | _, Quit -> Gt
    | Term, Term -> Eq

  include Comparable.Make (struct
    type nonrec t = t

    let compare = compare

    let to_dyn = to_dyn
  end)

  let all = [ Int; Quit; Term ]

  let to_int = function
    | Int -> Sys.sigint
    | Quit -> Sys.sigquit
    | Term -> Sys.sigterm

  let of_int =
    List.map all ~f:(fun t -> (to_int t, t))
    |> Int.Map.of_list_reduce ~f:(fun _ t -> t)
    |> Int.Map.find

  let name t = Signal.name (to_int t)
end

module Shutdown = struct
  module Signal = Signal

  module Reason = struct
    type t =
      | Requested
      | Signal of Signal.t
  end

  exception E of Reason.t
end

module Thread : sig
  val spawn : (unit -> 'a) -> unit

  val delay : float -> unit

  val wait_signal : int list -> int
end = struct
  include Thread

  let block_signals =
    lazy
      (let signos = List.map Signal.all ~f:Signal.to_int in
       ignore (Unix.sigprocmask SIG_BLOCK signos : int list))

  let create =
    if Sys.win32 then Thread.create
    else
      (* On unix, we make sure to block signals globally before starting a
         thread so that only the signal watcher thread can receive signals. *)
      fun f x ->
      Lazy.force block_signals;
      Thread.create f x

  let spawn f =
    let (_ : Thread.t) = create f () in
    ()
end

module Shutdown_reason = struct
  module T = struct
    type t =
      | Shutdown
      | Signal of Signal.t

    let to_dyn t =
      match t with
      | Shutdown -> Dyn.Variant ("Shutdown", [])
      | Signal signal -> Dyn.Variant ("Signal", [ Signal.to_dyn signal ])

    let compare a b =
      match (a, b) with
      | Shutdown, Shutdown -> Eq
      | Shutdown, _ -> Lt
      | _, Shutdown -> Gt
      | Signal a, Signal b -> Signal.compare a b
  end

  include T
  include Comparable.Make (T)
end

(** The event queue *)
module Event : sig
  type build_input_change =
    | Fs_event of Dune_file_watcher.Fs_memo_event.t
    | Invalidation of Memo.Invalidation.t

  type t =
    | File_watcher_task of (unit -> Dune_file_watcher.Event.t list)
    | Build_inputs_changed of build_input_change Nonempty_list.t
    | File_system_sync of Dune_file_watcher.Sync_id.t
    | File_system_watcher_terminated
    | Job_completed of job * Proc.Process_info.t
    | Shutdown of Shutdown_reason.t
    | Worker_task of Fiber.fill
    | Yield of unit Fiber.Ivar.t
    | Timer of Fiber.fill

  module Queue : sig
    type t

    type event

    val create : Dune_stats.t option -> t

    (** Return the next event. File changes event are always flattened and
        returned first. *)
    val next : t -> event

    (** Pending worker tasks *)
    val pending_worker_tasks : t -> int

    (** Register the fact that a job was started. *)
    val register_job_started : t -> unit

    (** Number of jobs for which the status hasn't been reported yet .*)
    val pending_jobs : t -> int

    val send_worker_task_completed : t -> Fiber.fill -> unit

    val register_worker_task_started : t -> unit

    val send_file_watcher_task :
      t -> (unit -> Dune_file_watcher.Event.t list) -> unit

    (** It's a bit weird to have both this and [send_file_watcher_task]. The
        reason is that [send_file_watcher_task] uses [send_file_watcher_events]
        internally. *)
    val send_file_watcher_events : t -> Dune_file_watcher.Event.t list -> unit

    val send_invalidation_event : t -> Memo.Invalidation.t -> unit

    val send_job_completed : t -> job -> Proc.Process_info.t -> unit

    val send_signal : t -> Shutdown_reason.t -> unit

    val send_timers_completed : t -> Fiber.fill Nonempty_list.t -> unit

    val yield_if_there_are_pending_events : t -> unit Fiber.t
  end
  with type event := t
end = struct
  type build_input_change =
    | Fs_event of Dune_file_watcher.Fs_memo_event.t
    | Invalidation of Memo.Invalidation.t

  type t =
    | File_watcher_task of (unit -> Dune_file_watcher.Event.t list)
    | Build_inputs_changed of build_input_change Nonempty_list.t
    | File_system_sync of Dune_file_watcher.Sync_id.t
    | File_system_watcher_terminated
    | Job_completed of job * Proc.Process_info.t
    | Shutdown of Shutdown_reason.t
    | Worker_task of Fiber.fill
    | Yield of unit Fiber.Ivar.t
    | Timer of Fiber.fill

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
      ; mutable signals : Shutdown_reason.Set.t
      ; mutex : Mutex.t
      ; cond : Condition.t
      ; mutable pending_jobs : int
      ; mutable pending_worker_tasks : int
      ; worker_tasks_completed : Fiber.fill Queue.t
      ; timers : Fiber.fill Queue.t
      ; stats : Dune_stats.t option
      ; mutable got_event : bool
      ; mutable yield : unit Fiber.Ivar.t option
      }

    let create stats =
      let jobs_completed = Queue.create () in
      let file_watcher_tasks = Queue.create () in
      let worker_tasks_completed = Queue.create () in
      let invalidation_events = [] in
      let signals = Shutdown_reason.Set.empty in
      let mutex = Mutex.create () in
      let cond = Condition.create () in
      let pending_jobs = 0 in
      let pending_worker_tasks = 0 in
      let timers = Queue.create () in
      { jobs_completed
      ; file_watcher_tasks
      ; invalidation_events
      ; timers
      ; signals
      ; mutex
      ; cond
      ; pending_jobs
      ; worker_tasks_completed
      ; pending_worker_tasks
      ; stats
      ; got_event = false
      ; yield = None
      }

    let register_job_started q = q.pending_jobs <- q.pending_jobs + 1

    let register_worker_task_started q =
      q.pending_worker_tasks <- q.pending_worker_tasks + 1

    let add_event q f =
      Mutex.lock q.mutex;
      f q;
      if not q.got_event then (
        q.got_event <- true;
        Condition.signal q.cond);
      Mutex.unlock q.mutex

    let yield_if_there_are_pending_events q =
      if Config.inside_dune || not q.got_event then Fiber.return ()
      else
        match q.yield with
        | Some ivar -> Fiber.Ivar.read ivar
        | None ->
          let ivar = Fiber.Ivar.create () in
          q.yield <- Some ivar;
          Fiber.Ivar.read ivar

    module Event_source : sig
      type queue := t

      type t

      val signal : t

      val file_watcher_task : t

      val invalidation : t

      val jobs_completed : t

      val worker_tasks_completed : t

      val yield : t

      val timers : t

      val chain : t list -> t

      val run : t -> queue -> event option
    end = struct
      type queue = t

      type t = queue -> event option

      let run t q = t q

      let signal : t =
       fun q ->
        Option.map (Shutdown_reason.Set.choose q.signals) ~f:(fun signal ->
            q.signals <- Shutdown_reason.Set.remove q.signals signal;
            Shutdown signal)

      let file_watcher_task q =
        Option.map (Queue.pop q.file_watcher_tasks) ~f:(fun job ->
            File_watcher_task job)

      let invalidation q =
        match q.invalidation_events with
        | [] -> None
        | events ->
          let rec process_events acc = function
            | [] ->
              q.invalidation_events <- [];
              Option.map
                (Nonempty_list.of_list (List.rev acc))
                ~f:(fun build_input_changes ->
                  Build_inputs_changed build_input_changes)
            | event :: events -> (
              match (event : Invalidation_event.t) with
              | Filesystem_event Watcher_terminated ->
                q.invalidation_events <- [];
                Some File_system_watcher_terminated
              | Filesystem_event (Sync id) -> (
                match Nonempty_list.of_list (List.rev acc) with
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
                     (Memo.Invalidation.clear_caches
                        ~reason:Event_queue_overflow)
                  :: acc)
                  events
              | Invalidation invalidation ->
                process_events (Invalidation invalidation :: acc) events)
          in
          process_events [] events

      let jobs_completed q =
        Option.map (Queue.pop q.jobs_completed) ~f:(fun (job, proc_info) ->
            q.pending_jobs <- q.pending_jobs - 1;
            assert (q.pending_jobs >= 0);
            Job_completed (job, proc_info))

      let worker_tasks_completed q =
        Option.map (Queue.pop q.worker_tasks_completed) ~f:(fun fill ->
            q.pending_worker_tasks <- q.pending_worker_tasks - 1;
            Worker_task fill)

      let yield q =
        Option.map q.yield ~f:(fun ivar ->
            q.yield <- None;
            Yield ivar)

      let timers q =
        Option.map (Queue.pop q.timers) ~f:(fun timer -> Timer timer)

      let chain list q = List.find_map list ~f:(fun f -> f q)
    end

    let next q =
      Option.iter q.stats ~f:Dune_stats.record_gc_and_fd;
      Mutex.lock q.mutex;
      let rec loop () =
        match
          Event_source.(
            run
              (chain
                 (* Event sources are listed in priority order. Signals are the
                    highest priority to maximize responsiveness to Ctrl+C.
                    [file_watcher_task], [worker_tasks_completed] and
                    [invalidation] are used for reacting to user input, so their
                    latency is also important. [jobs_completed] and [yield] are
                    where the bulk of the work is done, so they are the lowest
                    priority to avoid starving other things. *)
                 [ signal
                 ; file_watcher_task
                 ; invalidation
                 ; worker_tasks_completed
                 ; jobs_completed
                 ; yield
                 ; timers
                 ]))
            q
        with
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

    let send_worker_task_completed q event =
      add_event q (fun q -> Queue.push q.worker_tasks_completed event)

    let send_invalidation_events q events =
      add_event q (fun q ->
          q.invalidation_events <- q.invalidation_events @ events)

    let send_file_watcher_events q files =
      send_invalidation_events q
        (List.map files ~f:(fun file : Invalidation_event.t ->
             Filesystem_event file))

    let send_invalidation_event q invalidation =
      send_invalidation_events q [ Invalidation invalidation ]

    let send_job_completed q job proc_info =
      add_event q (fun q -> Queue.push q.jobs_completed (job, proc_info))

    let send_signal q signal =
      add_event q (fun q ->
          q.signals <- Shutdown_reason.Set.add q.signals signal)

    let send_file_watcher_task q job =
      add_event q (fun q -> Queue.push q.file_watcher_tasks job)

    let send_timers_completed q timers =
      add_event q (fun q ->
          Nonempty_list.to_list timers |> List.iter ~f:(Queue.push q.timers))

    let pending_jobs q = q.pending_jobs

    let pending_worker_tasks q = q.pending_worker_tasks
  end
end

module Event_queue = Event.Queue

let kill_process_group pid signal =
  match Sys.win32 with
  | false -> (
    (* Send to the entire process group so that any child processes created by
       the job are also terminated.

       Here we could consider sending a signal to the job process directly in
       addition to sending it to the process group. This is what GNU [timeout]
       does, for example.

       The upside would be that we deliver the signal to that process even if it
       changes its process group. This upside is small because moving between
       the process groups is a very unusual thing to do (creation of a new
       process group is not a problem for us, unlike for [timeout]).

       The downside is that it's more complicated, but also that by sending the
       signal twice we're greatly increasing the existing race condition where
       we call [wait] in parallel with [kill]. *)
    try Unix.kill (-Pid.to_int pid) signal with Unix.Unix_error _ -> ())
  | true -> (
    (* Process groups are not supported on Windows (or even if they are, [spawn]
       does not know how to use them), so we're only sending the signal to the
       job itself. *)
    try Unix.kill (Pid.to_int pid) signal with Unix.Unix_error _ -> ())

module Process_watcher : sig
  (** Initialize the process watcher thread. *)
  type t

  val init : Event.Queue.t -> t

  (** Register a new running job. *)
  val register_job : t -> job -> unit

  val is_running : t -> Pid.t -> bool

  (** Send the following signal to all running processes. *)
  val killall : t -> int -> unit
end = struct
  type process_state =
    | Running of job
    | Zombie of Proc.Process_info.t

  (* This mutable table is safe: it does not interact with the state we track in
     the build system. *)
  type t =
    { mutex : Mutex.t
    ; something_is_running : Condition.t
    ; table : (Pid.t, process_state) Table.t
    ; events : Event.Queue.t
    ; mutable running_count : int
    }

  let is_running t pid =
    Mutex.lock t.mutex;
    let res = Table.mem t.table pid in
    Mutex.unlock t.mutex;
    res

  module Process_table : sig
    val add : t -> job -> unit

    val remove : t -> Proc.Process_info.t -> unit

    val running_count : t -> int

    val iter : t -> f:(job -> unit) -> unit
  end = struct
    let add t job =
      match Table.find t.table job.pid with
      | None ->
        Table.set t.table job.pid (Running job);
        t.running_count <- t.running_count + 1;
        if t.running_count = 1 then Condition.signal t.something_is_running
      | Some (Zombie proc_info) ->
        Table.remove t.table job.pid;
        Event.Queue.send_job_completed t.events job proc_info
      | Some (Running _) -> assert false

    let remove t (proc_info : Proc.Process_info.t) =
      match Table.find t.table proc_info.pid with
      | None -> Table.set t.table proc_info.pid (Zombie proc_info)
      | Some (Running job) ->
        t.running_count <- t.running_count - 1;
        Table.remove t.table proc_info.pid;
        Event.Queue.send_job_completed t.events job proc_info
      | Some (Zombie _) -> assert false

    let iter t ~f =
      Table.iter t.table ~f:(fun data ->
          match data with
          | Running job -> f job
          | Zombie _ -> ())

    let running_count t = t.running_count
  end

  let register_job t job =
    Event.Queue.register_job_started t.events;
    Mutex.lock t.mutex;
    Process_table.add t job;
    Mutex.unlock t.mutex

  let killall t signal =
    Mutex.lock t.mutex;
    Process_table.iter t ~f:(fun job -> kill_process_group job.pid signal);
    Mutex.unlock t.mutex

  exception Finished of Proc.Process_info.t

  let wait_nonblocking_win32 t =
    try
      Process_table.iter t ~f:(fun job ->
          let pid, status = Unix.waitpid [ WNOHANG ] (Pid.to_int job.pid) in
          if pid <> 0 then
            let now = Unix.gettimeofday () in
            let info : Proc.Process_info.t =
              { pid = Pid.of_int pid
              ; status
              ; end_time = now
              ; resource_usage = None
              }
            in
            raise_notrace (Finished info));
      false
    with Finished proc_info ->
      (* We need to do the [Unix.waitpid] and remove the process while holding
         the lock, otherwise the pid might be reused in between. *)
      Process_table.remove t proc_info;
      true

  let wait_win32 t =
    while not (wait_nonblocking_win32 t) do
      Mutex.unlock t.mutex;
      Thread.delay 0.001;
      Mutex.lock t.mutex
    done

  let wait_unix t =
    Mutex.unlock t.mutex;
    let proc_info = Proc.wait [] in
    Mutex.lock t.mutex;
    Process_table.remove t proc_info

  let wait = if Sys.win32 then wait_win32 else wait_unix

  let run t =
    Mutex.lock t.mutex;
    while true do
      while Process_table.running_count t = 0 do
        Condition.wait t.something_is_running t.mutex
      done;
      wait t
    done

  let init events =
    let t =
      { mutex = Mutex.create ()
      ; something_is_running = Condition.create ()
      ; table = Table.create (module Pid) 128
      ; events
      ; running_count = 0
      }
    in
    Thread.spawn (fun () -> run t);
    t
end

module Signal_watcher : sig
  val init : Event.Queue.t -> unit
end = struct
  let signos = List.map Signal.all ~f:Signal.to_int

  let warning =
    {|

**************************************************************
* Press Control+C again quickly to perform an emergency exit *
**************************************************************

|}

  external sys_exit : int -> _ = "caml_sys_exit"

  let signal_waiter () =
    if Sys.win32 then (
      let r, w = Unix.pipe ~cloexec:true () in
      let buf = Bytes.create 1 in
      Sys.set_signal Sys.sigint
        (Signal_handle (fun _ -> assert (Unix.write w buf 0 1 = 1)));
      Staged.stage (fun () ->
          assert (Unix.read r buf 0 1 = 1);
          Signal.Int))
    else
      Staged.stage (fun () ->
          Thread.wait_signal signos |> Signal.of_int |> Option.value_exn)

  let run q =
    let last_exit_signals = Queue.create () in
    let wait_signal = Staged.unstage (signal_waiter ()) in
    while true do
      let signal = wait_signal () in
      Event.Queue.send_signal q (Signal signal);
      match signal with
      | Int | Quit | Term ->
        let now = Unix.gettimeofday () in
        Queue.push last_exit_signals now;
        (* Discard old signals *)
        while
          Queue.length last_exit_signals >= 0
          && now -. Queue.peek_exn last_exit_signals > 1.
        do
          ignore (Queue.pop_exn last_exit_signals : float)
        done;
        let n = Queue.length last_exit_signals in
        if n = 2 then prerr_endline warning;
        if n = 3 then sys_exit 1
    done

  let init q = Thread.spawn (fun () -> run q)
end

type status =
  | (* We are not doing a build. Just accumulating invalidations until the next
       build starts. *)
    Standing_by of
      { invalidation : Memo.Invalidation.t
      ; saw_insignificant_changes : bool
            (* When [insignificant_changes = `Ignore], this field is always
               false.

               When [insignificant_changes = `React], we do the following:

               Whether we saw build input changes that are insignificant for
               the build. We need to track this because we still want to start
               a new build in this case, even if we know it's going to be a
               no-op. We do that so that RPC clients can observe that Dune
               reacted to the change. *)
      }
  | (* Running a build *)
    Building of Fiber.Cancel.t
  | (* Cancellation requested. Build jobs are immediately rejected in this
       state *)
    Restarting_build of
      Memo.Invalidation.t

module Build_outcome = struct
  type t =
    | Success
    | Failure
end

module Handler = struct
  module Event = struct
    type t =
      | Tick
      | Source_files_changed of { details_hum : string list }
      | Build_interrupted
      | Build_finish of Build_outcome.t
  end

  type t = Config.t -> Event.t -> unit
end

module Alarm_clock : sig
  type t

  val create : Event.Queue.t -> frequency:float -> t

  type alarm

  val await : alarm -> [ `Finished | `Cancelled ] Fiber.t

  val cancel : t -> alarm -> unit

  val sleep : t -> float -> alarm

  val close : t -> unit
end = struct
  type alarm = [ `Finished | `Cancelled ] Fiber.Ivar.t

  type t =
    { events : Event.Queue.t
    ; mutex : Mutex.t
    ; frequency : float
    ; mutable alarms : (float * [ `Finished | `Cancelled ] Fiber.Ivar.t) list
    ; mutable active : bool
    }

  let await = Fiber.Ivar.read

  let cancel t alarm =
    Mutex.lock t.mutex;
    let found = ref false in
    t.alarms <-
      List.filter t.alarms ~f:(fun (_, alarm') ->
          let eq = alarm' == alarm in
          if eq then found := true;
          not eq);
    Mutex.unlock t.mutex;
    if !found then
      Event.Queue.send_timers_completed t.events
        [ Fiber.Fill (alarm, `Cancelled) ]

  let polling_loop t () =
    let rec loop () =
      match t.active with
      | false -> ()
      | true ->
        let now = Unix.gettimeofday () in
        let expired, active =
          List.partition_map t.alarms ~f:(fun (expiration, ivar) ->
              if now > expiration then Left (Fiber.Fill (ivar, `Finished))
              else Right (expiration, ivar))
        in
        t.alarms <- active;
        Mutex.unlock t.mutex;
        (match Nonempty_list.of_list expired with
        | None -> ()
        | Some expired -> Event.Queue.send_timers_completed t.events expired);
        Thread.delay t.frequency;
        Mutex.lock t.mutex;
        loop ()
    in
    Mutex.lock t.mutex;
    loop ();
    t.alarms <- [];
    Mutex.unlock t.mutex

  let create events ~frequency =
    let t =
      { events; active = true; alarms = []; frequency; mutex = Mutex.create () }
    in
    Thread.spawn (polling_loop t);
    t

  let sleep t duration =
    Mutex.lock t.mutex;
    let ivar = Fiber.Ivar.create () in
    if not t.active then (
      Mutex.unlock t.mutex;
      Code_error.raise "cannot schedule timers after close" []);
    t.alarms <- (duration +. Unix.gettimeofday (), ivar) :: t.alarms;
    Mutex.unlock t.mutex;
    ivar

  let close t =
    Mutex.lock t.mutex;
    t.active <- false;
    Mutex.unlock t.mutex
end

(** All fields of [t] must be immutable. This is because we re-create [t]
    everytime we start a new build to locally set the [cancel] field. However,
    all instances of [t] must share all other fields, in particular the
    references such as [status].

    Another option would be to split [t] in two records such as:

    {[
      type shared = { blah : blah }

      type t =
        { shared : shared
        ; cancel : Fiber.Cancel.t
        }
    ]}

    However, this style is more heavy. What is more, it means that every access
    to shared fields of [t] is now slower.

    With the current approach setting the cancellation is slower as it requires
    copying almost all fields of [t]. However this happens only once per build
    and a build is a much heavier operation than copying this record, so it's
    fine. *)
type t =
  { config : Config.t
  ; alarm_clock : Alarm_clock.t Lazy.t
  ; status : status ref
  ; handler : Handler.t
  ; job_throttle : Fiber.Throttle.t
  ; events : Event.Queue.t
  ; process_watcher : Process_watcher.t
  ; file_watcher : Dune_file_watcher.t option
  ; fs_syncs : unit Fiber.Ivar.t Dune_file_watcher.Sync_id.Table.t
  ; wait_for_build_input_change : unit Fiber.Ivar.t option ref
  ; cancel : Fiber.Cancel.t
  }

let t : t Fiber.Var.t = Fiber.Var.create ()

let set x f = Fiber.Var.set t x f

let t_opt () = Fiber.Var.get t

let t () = Fiber.Var.get_exn t

let running_jobs_count t = Event.Queue.pending_jobs t.events

let yield_if_there_are_pending_events () =
  t_opt () >>= function
  | None -> Fiber.return ()
  | Some t -> Event.Queue.yield_if_there_are_pending_events t.events

exception Build_cancelled

let cancelled () = raise (Memo.Non_reproducible Build_cancelled)

let check_cancelled t = if Fiber.Cancel.fired t.cancel then cancelled ()

let abort_if_build_was_cancelled = t_opt () >>| Option.iter ~f:check_cancelled

let check_point =
  t_opt () >>= function
  | None -> Fiber.return ()
  | Some t ->
    (* CR-someday amokhov: we used to call [check_cancelled t] here but that led
       to a significant performance regression. Raising [Build_cancelled] saves
       some unnecessary recomputation but also destroys early cutoffs. We should
       change Memo to store previous successes to make such early cancellations
       preserve the early cutoff behaviour. *)
    Event.Queue.yield_if_there_are_pending_events t.events

let () = Memo.check_point := check_point

let with_job_slot f =
  let* t = t () in
  Fiber.Throttle.run t.job_throttle ~f:(fun () ->
      check_cancelled t;
      f t.cancel t.config)

(* We use this version privately in this module whenever we can pass the
   scheduler explicitly *)
let wait_for_process t pid =
  let+ res, outcome =
    Fiber.Cancel.with_handler t.cancel
      ~on_cancel:(fun () ->
        Process_watcher.killall t.process_watcher Sys.sigkill;
        Fiber.return ())
      (fun () ->
        let ivar = Fiber.Ivar.create () in
        Process_watcher.register_job t.process_watcher { pid; ivar };
        Fiber.Ivar.read ivar)
  in
  match outcome with
  | Cancelled () -> cancelled ()
  | Not_cancelled -> res

let got_signal signal =
  if !Log.verbose then
    match (signal : Shutdown_reason.t) with
    | Shutdown -> Log.info [ Pp.textf "Shutting down." ]
    | Signal signal ->
      Log.info [ Pp.textf "Got signal %s, exiting." (Signal.name signal) ]

let filesystem_watcher_terminated () =
  Log.info [ Pp.textf "Filesystem watcher terminated, exiting." ]

type saw_signal =
  | Ok
  | Got_signal

let kill_and_wait_for_all_processes t =
  Process_watcher.killall t.process_watcher Sys.sigkill;
  let saw_signal = ref Ok in
  while Event.Queue.pending_jobs t.events > 0 do
    match Event.Queue.next t.events with
    | Shutdown signal ->
      got_signal signal;
      saw_signal := Got_signal
    | _ -> ()
  done;
  !saw_signal

let prepare (config : Config.t) ~(handler : Handler.t) =
  let events = Event.Queue.create config.stats in
  (* We return the scheduler in chunks to resolve the dependency cycle
     (scheduler wants to know the file_watcher, file_watcher wants to send
     events to scheduler) *)
  ( events
  , fun ~file_watcher ->
      (* The signal watcher must be initialized first so that signals are
         blocked in all threads. *)
      Signal_watcher.init events;
      let process_watcher = Process_watcher.init events in
      { status =
          (* Slightly weird initialization happening here: for polling mode we
             initialize in "Building" state, immediately switch to Standing_by
             and then back to "Building". It would make more sense to start in
             "Stand_by" from the start. We can't "just" switch the initial value
             here because then the non-polling mode would run in "Standing_by"
             mode, which is even weirder. *)
          ref (Building (Fiber.Cancel.create ()))
      ; job_throttle = Fiber.Throttle.create config.concurrency
      ; process_watcher
      ; events
      ; config
      ; handler
      ; file_watcher
      ; fs_syncs = Dune_file_watcher.Sync_id.Table.create 64
      ; wait_for_build_input_change = ref None
      ; alarm_clock = lazy (Alarm_clock.create events ~frequency:0.1)
      ; cancel =
          (* This cancellation will never be fired, so this field could instead
             be an [option]. We use a dummy cancellation rather than an option
             to keep the code simpler. *)
          Fiber.Cancel.create ()
      } )

module Run_once : sig
  type run_error =
    | Already_reported
    | Shutdown_requested of Shutdown.Reason.t
    | Exn of Exn_with_backtrace.t

  (** Run the build and clean up after it (kill any stray processes etc). *)
  val run_and_cleanup : t -> (unit -> 'a Fiber.t) -> ('a, run_error) Result.t
end = struct
  type run_error =
    | Already_reported
    | Shutdown_requested of Shutdown.Reason.t
    | Exn of Exn_with_backtrace.t

  exception Abort of run_error

  let handle_invalidation_events events =
    let handle_event event =
      match (event : Event.build_input_change) with
      | Invalidation invalidation -> invalidation
      | Fs_event event -> Fs_memo.handle_fs_event event
    in
    let events = Nonempty_list.to_list events in
    List.fold_left events ~init:Memo.Invalidation.empty ~f:(fun acc event ->
        Memo.Invalidation.combine acc (handle_event event))

  (** This function is the heart of the scheduler. It makes progress in
      executing fibers by doing the following:

      - notifying completed jobs
      - starting cancellations
      - terminating the scheduler on signals *)
  let rec iter (t : t) : Fiber.fill Nonempty_list.t =
    t.handler t.config Tick;
    match Event.Queue.next t.events with
    | Job_completed (job, proc_info) -> [ Fill (job.ivar, proc_info) ]
    | File_watcher_task job ->
      let events = job () in
      Event.Queue.send_file_watcher_events t.events events;
      iter t
    | File_system_sync id -> (
      match Dune_file_watcher.Sync_id.Table.find t.fs_syncs id with
      | None -> iter t
      | Some ivar ->
        Dune_file_watcher.Sync_id.Table.remove t.fs_syncs id;
        [ Fill (ivar, ()) ])
    | Build_inputs_changed events -> build_input_change t events
    | Timer fill | Worker_task fill -> [ fill ]
    | File_system_watcher_terminated ->
      filesystem_watcher_terminated ();
      raise (Abort Already_reported)
    | Yield ivar -> [ Fill (ivar, ()) ]
    | Shutdown signal ->
      got_signal signal;
      raise
      @@ Abort
           (Shutdown_requested
              (match signal with
              | Shutdown -> Requested
              | Signal s -> Signal s))

  and build_input_change (t : t) events =
    let invalidation = handle_invalidation_events events in
    let fills =
      if Memo.Invalidation.is_empty invalidation then
        match !(t.status) with
        | Standing_by prev ->
          (match t.config.insignificant_changes with
          | `Ignore -> ()
          | `React ->
            t.status :=
              Standing_by { prev with saw_insignificant_changes = true });
          []
        | _ -> []
      else
        match !(t.status) with
        | Restarting_build prev_invalidation ->
          t.status :=
            Restarting_build
              (Memo.Invalidation.combine prev_invalidation invalidation);
          []
        | Standing_by prev ->
          t.status :=
            Standing_by
              { prev with
                invalidation =
                  Memo.Invalidation.combine prev.invalidation invalidation
              };
          []
        | Building cancellation ->
          t.handler t.config Build_interrupted;
          t.status := Restarting_build invalidation;
          Fiber.Cancel.fire' cancellation
    in
    match
      Nonempty_list.of_list
      @@
      match !(t.wait_for_build_input_change) with
      | None -> fills
      | Some ivar -> (
        match
          if Memo.Invalidation.is_empty invalidation then
            match t.config.insignificant_changes with
            | `Ignore -> `Keep_waiting
            | `React -> `Fill_waiter
          else `Fill_waiter
        with
        | `Keep_waiting -> fills
        | `Fill_waiter ->
          t.wait_for_build_input_change := None;
          Fiber.Fill (ivar, ()) :: fills)
    with
    | None -> iter t
    | Some fills -> fills

  let run t f : _ result =
    let fiber =
      set t (fun () ->
          Fiber.map_reduce_errors
            (module Monoid.Unit)
            f
            ~on_error:(fun e ->
              Dune_util.Report_error.report e;
              Fiber.return ()))
    in
    match Fiber.run fiber ~iter:(fun () -> iter t) with
    | Ok res ->
      assert (Event.Queue.pending_jobs t.events = 0);
      assert (Event.Queue.pending_worker_tasks t.events = 0);
      Ok res
    | Error () -> Error Already_reported
    | exception Abort err -> Error err
    | exception exn -> Error (Exn (Exn_with_backtrace.capture exn))

  let run_and_cleanup t f =
    let res = run t f in
    Console.Status_line.clear ();
    match kill_and_wait_for_all_processes t with
    | Got_signal -> Error Already_reported
    | Ok -> res
end

module Worker = struct
  type t =
    { worker : Thread_worker.t
    ; events : Event.Queue.t
    }

  let stop t = Thread_worker.stop t.worker

  let create () =
    let worker = Thread_worker.create ~spawn_thread:Thread.spawn in
    let+ scheduler = t () in
    { worker; events = scheduler.events }

  let task (t : t) ~f =
    let ivar = Fiber.Ivar.create () in
    let f () =
      let res = Exn_with_backtrace.try_with f in
      Event.Queue.send_worker_task_completed t.events (Fiber.Fill (ivar, res))
    in
    match Thread_worker.add_work t.worker ~f with
    | Error `Stopped -> Fiber.return (Error `Stopped)
    | Ok () -> (
      Event.Queue.register_worker_task_started t.events;
      let+ res = Fiber.Ivar.read ivar in
      match res with
      | Error exn -> Error (`Exn exn)
      | Ok e -> Ok e)

  let task_exn t ~f =
    let+ res = task t ~f in
    match res with
    | Ok a -> a
    | Error `Stopped ->
      Code_error.raise "Scheduler.Worker.task_exn: worker stopped" []
    | Error (`Exn e) -> Exn_with_backtrace.reraise e
end

let flush_file_watcher t =
  match t.file_watcher with
  | None -> Fiber.return ()
  | Some file_watcher ->
    let ivar = Fiber.Ivar.create () in
    let id = Dune_file_watcher.emit_sync file_watcher in
    Dune_file_watcher.Sync_id.Table.set t.fs_syncs id ivar;
    Fiber.Ivar.read ivar

let wait_for_build_input_change t =
  match !(t.wait_for_build_input_change) with
  | Some ivar -> Fiber.Ivar.read ivar
  | None -> (
    match !(t.status) with
    | Standing_by { invalidation; saw_insignificant_changes }
      when (not (Memo.Invalidation.is_empty invalidation))
           || saw_insignificant_changes -> Fiber.return ()
    | Restarting_build _ -> Fiber.return ()
    | Standing_by _ | Building _ ->
      let ivar = Fiber.Ivar.create () in
      t.wait_for_build_input_change := Some ivar;
      Fiber.Ivar.read ivar)

module Run = struct
  exception Build_cancelled = Build_cancelled

  module Shutdown = Shutdown

  type file_watcher =
    | Automatic
    | No_watcher

  module Build_outcome = Build_outcome
  module Event_queue = Event.Queue
  module Event = Handler.Event

  let rec poll_iter t step ~invalidation =
    let cancel = Fiber.Cancel.create () in
    t.status := Building cancel;
    (if Memo.Invalidation.is_empty invalidation then Memo.Perf_counters.reset ()
    else
      let details_hum = Memo.Invalidation.details_hum invalidation in
      t.handler t.config (Source_files_changed { details_hum });
      Memo.reset invalidation);
    let* res = set { t with cancel } (fun () -> step) in
    match !(t.status) with
    | Standing_by _ ->
      (* We just finished a build, so there's no way this was set *)
      assert false
    | Restarting_build invalidation -> poll_iter t step ~invalidation
    | Building _ ->
      let res : Build_outcome.t =
        match res with
        | Error `Already_reported -> Failure
        | Ok () -> Success
      in
      t.status :=
        Standing_by
          { invalidation = Memo.Invalidation.empty
          ; saw_insignificant_changes = false
          };
      t.handler t.config (Build_finish res);
      Fiber.return res

  let poll_iter t step =
    match !(t.status) with
    | Building _ | Restarting_build _ -> assert false
    | Standing_by { invalidation; _ } -> poll_iter t step ~invalidation

  type step = (unit, [ `Already_reported ]) Result.t Fiber.t

  let poll_init () =
    let+ t = t () in
    assert (
      match !(t.status) with
      | Building _ -> true
      | _ -> false);
    t.status :=
      Standing_by
        { invalidation = Memo.Invalidation.empty
        ; saw_insignificant_changes = false
        };
    t

  let poll step =
    let* t = poll_init () in
    let rec loop () =
      let* _res = poll_iter t step in
      let* () = wait_for_build_input_change t in
      loop ()
    in
    loop ()

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

  let go config ?timeout ?(file_watcher = No_watcher)
      ~(on_event : Config.t -> Handler.Event.t -> unit) run =
    let events, prepare = prepare config ~handler:on_event in
    let file_watcher =
      match file_watcher with
      | No_watcher -> None
      | Automatic ->
        Some
          (Dune_file_watcher.create_default
             ~scheduler:
               { spawn_thread = Thread.spawn
               ; thread_safe_send_emit_events_job =
                   (fun job -> Event_queue.send_file_watcher_task events job)
               }
             ())
    in
    let t = prepare ~file_watcher in
    let initial_invalidation = Fs_memo.init ~dune_file_watcher:file_watcher in
    Memo.reset initial_invalidation;
    let result =
      let run =
        match timeout with
        | None -> run
        | Some timeout ->
          fun () ->
            let sleep = Alarm_clock.sleep (Lazy.force t.alarm_clock) timeout in
            Fiber.fork_and_join_unit
              (fun () ->
                let+ res = Alarm_clock.await sleep in
                match res with
                | `Finished -> Event_queue.send_signal t.events Shutdown
                | `Cancelled -> ())
              (fun () ->
                Fiber.finalize run ~finally:(fun () ->
                    Alarm_clock.cancel (Lazy.force t.alarm_clock) sleep;
                    Fiber.return ()))
      in
      match Run_once.run_and_cleanup t run with
      | Ok a -> Result.Ok a
      | Error (Shutdown_requested reason) -> Error (Shutdown.E reason, None)
      | Error Already_reported ->
        Error (Dune_util.Report_error.Already_reported, None)
      | Error (Exn exn_with_bt) ->
        Error (exn_with_bt.exn, Some exn_with_bt.backtrace)
    in
    Option.iter file_watcher ~f:(fun watcher ->
        match Dune_file_watcher.shutdown watcher with
        | `Kill pid -> ignore (wait_for_process t pid : _ Fiber.t)
        | `Thunk f -> f ()
        | `No_op -> ());
    ignore (kill_and_wait_for_all_processes t : saw_signal);
    if Lazy.is_val t.alarm_clock then
      Alarm_clock.close (Lazy.force t.alarm_clock);
    match result with
    | Ok a -> a
    | Error (exn, None) -> Exn.raise exn
    | Error (exn, Some bt) -> Exn.raise_with_backtrace exn bt
end

let shutdown () =
  let+ t = t () in
  Event.Queue.send_signal t.events Shutdown

let inject_memo_invalidation invalidation =
  let* t = t () in
  Event.Queue.send_invalidation_event t.events invalidation;
  Fiber.return ()

let wait_for_process_with_timeout t pid ~timeout ~is_process_group_leader =
  Fiber.of_thunk (fun () ->
      let sleep = Alarm_clock.sleep (Lazy.force t.alarm_clock) timeout in
      Fiber.fork_and_join_unit
        (fun () ->
          let+ res = Alarm_clock.await sleep in
          if res = `Finished && Process_watcher.is_running t.process_watcher pid
          then
            if is_process_group_leader then kill_process_group pid Sys.sigkill
            else Unix.kill (Pid.to_int pid) Sys.sigkill)
        (fun () ->
          let+ res = wait_for_process t pid in
          Alarm_clock.cancel (Lazy.force t.alarm_clock) sleep;
          res))

let wait_for_process ?timeout ?(is_process_group_leader = false) pid =
  let* t = t () in
  match timeout with
  | None -> wait_for_process t pid
  | Some timeout ->
    wait_for_process_with_timeout t pid ~timeout ~is_process_group_leader

let sleep duration =
  let* t = t () in
  let alarm_clock = Lazy.force t.alarm_clock in
  let+ res = Alarm_clock.await (Alarm_clock.sleep alarm_clock duration) in
  match res with
  | `Finished -> ()
  | `Cancelled ->
    (* cancellation mechanism isn't exposed to the user *)
    assert false

let flush_file_watcher () =
  let* t = t () in
  flush_file_watcher t

let wait_for_build_input_change () =
  let* t = t () in
  wait_for_build_input_change t
