let on_error e =
  Report_error.report e;
  Fiber.return ()

open! Stdune
open Import
open Fiber.O

module Config = struct
  include Config

  module Display = struct
    type t =
      | Progress
      | Short
      | Verbose
      | Quiet

    let all =
      [ ("progress", Progress)
      ; ("verbose", Verbose)
      ; ("short", Short)
      ; ("quiet", Quiet)
      ]

    let to_dyn = function
      | Progress -> Dyn.Variant ("Progress", [])
      | Quiet -> Variant ("Quiet", [])
      | Short -> Variant ("Short", [])
      | Verbose -> Variant ("Verbose", [])

    let console_backend = function
      | Progress -> Console.Backend.progress
      | Short
      | Verbose
      | Quiet ->
        Console.Backend.dumb
  end

  type t =
    { concurrency : int
    ; display : Display.t
    ; rpc : Dune_rpc.Where.t option
    ; stats : Stats.t option
    }

  let add_to_env t env =
    match t.rpc with
    | None -> env
    | Some where -> Dune_rpc.Where.add_to_env where env
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

  let compare : t -> t -> Ordering.t = Poly.compare

  include Comparable.Make (struct
    type nonrec t = t

    let compare = compare

    let to_dyn _ = Dyn.opaque
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

module Thread = struct
  include Thread

  let block_signals =
    lazy
      (let signos = List.map Signal.all ~f:Signal.to_int in
       ignore (Unix.sigprocmask SIG_BLOCK signos : int list))

  let create =
    if Sys.win32 then
      Thread.create
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

(** The event queue *)
module Event : sig
  type t =
    | File_system_changed of Fs_memo.Event.t Nonempty_list.t
    | Job_completed of job * Proc.Process_info.t
    | Signal of Signal.t
    | Rpc of Fiber.fill

  module Queue : sig
    type t

    type event

    val create : Stats.t option -> t

    (** Return the next event. File changes event are always flattened and
        returned first. *)
    val next : t -> event

    (** Handle all enqueued deduplications. *)
    val flush_sync_tasks : t -> unit

    (** Ignore the ne next file change event about this file. *)
    val ignore_next_file_change_event : t -> Path.t -> unit

    (** Pending rpc events *)
    val pending_rpc : t -> int

    (** Register the fact that a job was started. *)
    val register_job_started : t -> unit

    (** Number of jobs for which the status hasn't been reported yet .*)
    val pending_jobs : t -> int

    val send_rpc_completed : t -> Fiber.fill -> unit

    val register_rpc_started : t -> unit

    (** Send an event to the main thread. *)
    val send_files_changed : t -> Path.t list -> unit

    val send_job_completed : t -> job -> Proc.Process_info.t -> unit

    val send_signal : t -> Signal.t -> unit

    val send_sync_task : t -> (unit -> unit) -> unit
  end
  with type event := t
end = struct
  type t =
    | File_system_changed of Fs_memo.Event.t Nonempty_list.t
    | Job_completed of job * Proc.Process_info.t
    | Signal of Signal.t
    | Rpc of Fiber.fill

  module Queue = struct
    type t =
      { jobs_completed : (job * Proc.Process_info.t) Queue.t
      ; sync_tasks : (unit -> unit) Queue.t
      ; mutable files_changed : Path.t list
      ; mutable signals : Signal.Set.t
      ; mutex : Mutex.t
      ; cond : Condition.t
            (* CR-soon amokhov: The way we handle "ignored files" using this
               mutable table is fragile and also wrong. We use [ignored_files]
               for the [(mode promote)] feature: if a file is promoted, we call
               [ignore_next_file_change_event] so that the upcoming file-change
               event does not invalidate the current build. However, instead of
               ignoring the events, we should merely postpone them and restart
               the build to take the promoted files into account if need be. *)
      ; ignored_files : (string, unit) Table.t
      ; mutable pending_jobs : int
      ; mutable pending_rpc : int
      ; rpc_completed : Fiber.fill Queue.t
      ; stats : Stats.t option
      }

    let create stats =
      let jobs_completed = Queue.create () in
      let rpc_completed = Queue.create () in
      let sync_tasks = Queue.create () in
      let files_changed = [] in
      let signals = Signal.Set.empty in
      let mutex = Mutex.create () in
      let cond = Condition.create () in
      let ignored_files = Table.create (module String) 64 in
      let pending_jobs = 0 in
      let pending_rpc = 0 in
      { jobs_completed
      ; sync_tasks
      ; files_changed
      ; signals
      ; mutex
      ; cond
      ; ignored_files
      ; pending_jobs
      ; rpc_completed
      ; pending_rpc
      ; stats
      }

    let register_job_started q = q.pending_jobs <- q.pending_jobs + 1

    let register_rpc_started q = q.pending_rpc <- q.pending_rpc + 1

    let ignore_next_file_change_event q path =
      assert (Path.is_in_source_tree path);
      Table.set q.ignored_files (Path.to_absolute_filename path) ()

    let available q =
      not
        (List.is_empty q.files_changed
        && Queue.is_empty q.jobs_completed
        && Signal.Set.is_empty q.signals
        && Queue.is_empty q.sync_tasks
        && Queue.is_empty q.rpc_completed)

    let sync_task q =
      match Queue.pop q.sync_tasks with
      | None -> false
      | Some task ->
        task ();
        true

    let rec flush_sync_tasks q = if sync_task q then flush_sync_tasks q

    let next q =
      Option.iter q.stats ~f:Stats.record_gc_and_fd;
      Mutex.lock q.mutex;
      let rec loop () =
        while not (available q) do
          Condition.wait q.cond q.mutex
        done;
        if sync_task q then
          loop ()
        else
          match Signal.Set.choose q.signals with
          | Some signal ->
            q.signals <- Signal.Set.remove q.signals signal;
            Signal signal
          | None -> (
            match q.files_changed with
            | [] -> (
              match Queue.pop q.jobs_completed with
              | None ->
                q.pending_rpc <- q.pending_rpc - 1;
                Rpc (Queue.pop_exn q.rpc_completed)
              | Some (job, proc_info) ->
                q.pending_jobs <- q.pending_jobs - 1;
                assert (q.pending_jobs >= 0);
                Job_completed (job, proc_info))
            | files_changed -> (
              q.files_changed <- [];
              let events =
                List.filter_map files_changed ~f:(fun path ->
                    let abs_path = Path.to_absolute_filename path in
                    if Table.mem q.ignored_files abs_path then (
                      (* only use ignored record once *)
                      Table.remove q.ignored_files abs_path;
                      None
                    ) else
                      (* CR-soon amokhov: Generate more precise events. *)
                      Some (Fs_memo.Event.create ~kind:Unknown ~path))
              in
              match Nonempty_list.of_list events with
              | None -> loop ()
              | Some events -> File_system_changed events))
      in
      let ev = loop () in
      Mutex.unlock q.mutex;
      ev

    let send_rpc_completed q event =
      Mutex.lock q.mutex;
      let avail = available q in
      Queue.push q.rpc_completed event;
      if not avail then Condition.signal q.cond;
      Mutex.unlock q.mutex

    let send_files_changed q files =
      Mutex.lock q.mutex;
      let avail = available q in
      q.files_changed <- List.rev_append files q.files_changed;
      if not avail then Condition.signal q.cond;
      Mutex.unlock q.mutex

    let send_job_completed q job proc_info =
      Mutex.lock q.mutex;
      let avail = available q in
      Queue.push q.jobs_completed (job, proc_info);
      if not avail then Condition.signal q.cond;
      Mutex.unlock q.mutex

    let send_signal q signal =
      Mutex.lock q.mutex;
      let avail = available q in
      q.signals <- Signal.Set.add q.signals signal;
      if not avail then Condition.signal q.cond;
      Mutex.unlock q.mutex

    let send_sync_task q f =
      Mutex.lock q.mutex;
      let avail = available q in
      Queue.push q.sync_tasks f;
      if not avail then Condition.signal q.cond;
      Mutex.unlock q.mutex

    let pending_jobs q = q.pending_jobs

    let pending_rpc q = q.pending_rpc
  end
end

module File_watcher : sig
  type t

  (** Create a new file watcher. *)
  val create : Event.Queue.t -> t

  (** Pid of the external file watcher process *)
  val pid : t -> Pid.t
end = struct
  type t = Pid.t

  let pid t = t

  let command =
    lazy
      (let excludes =
         [ {|/_build|}
         ; {|/_opam|}
         ; {|/_esy|}
         ; {|/\..+|}
         ; {|~$|}
         ; {|/#[^#]*#$|}
         ; {|4913|} (* https://github.com/neovim/neovim/issues/3460 *)
         ]
       in
       let path = Path.to_string_maybe_quoted Path.root in
       match
         if Sys.linux then
           Bin.which ~path:(Env.path Env.initial) "inotifywait"
         else
           None
       with
       | Some inotifywait ->
         (* On Linux, use inotifywait. *)
         let excludes = String.concat ~sep:"|" excludes in
         ( inotifywait
         , [ "-r"
           ; path
           ; "--exclude"
           ; excludes
           ; "-e"
           ; "close_write"
           ; "-e"
           ; "delete"
           ; "--format"
           ; "%w%f"
           ; "-m"
           ; "-q"
           ] )
       | None -> (
         (* On all other platforms, try to use fswatch. fswatch's event
            filtering is not reliable (at least on Linux), so don't try to use
            it, instead act on all events. *)
         match Bin.which ~path:(Env.path Env.initial) "fswatch" with
         | Some fswatch ->
           let excludes =
             List.concat_map excludes ~f:(fun x -> [ "--exclude"; x ])
           in
           ( fswatch
           , [ "-r"
             ; path
             ; "--event"
             ; "Created"
             ; "--event"
             ; "Updated"
             ; "--event"
             ; "Removed"
             ]
             @ excludes )
         | None ->
           User_error.raise
             [ Pp.text
                 (if Sys.linux then
                   "Please install inotifywait to enable watch mode. If \
                    inotifywait is unavailable, fswatch may also be used but \
                    will result in a worse experience."
                 else
                   "Please install fswatch to enable watch mode.")
             ]))

  let buffering_time = 0.5 (* seconds *)

  let buffer_capacity = 65536

  type buffer =
    { data : Bytes.t
    ; mutable size : int
    }

  let read_lines buffer fd =
    let len =
      Unix.read fd buffer.data buffer.size (buffer_capacity - buffer.size)
    in
    buffer.size <- buffer.size + len;
    let lines = ref [] in
    let line_start = ref 0 in
    for i = 0 to buffer.size - 1 do
      let c = Bytes.get buffer.data i in
      if c = '\n' || c = '\r' then (
        (if !line_start < i then
          let line =
            Bytes.sub_string buffer.data ~pos:!line_start ~len:(i - !line_start)
          in
          lines := line :: !lines);
        line_start := i + 1
      )
    done;
    buffer.size <- buffer.size - !line_start;
    Bytes.blit ~src:buffer.data ~src_pos:!line_start ~dst:buffer.data ~dst_pos:0
      ~len:buffer.size;
    List.rev !lines

  let spawn_external_watcher () =
    let prog, args = Lazy.force command in
    let prog = Path.to_absolute_filename prog in
    let argv = prog :: args in
    let r, w = Unix.pipe () in
    let pid = Spawn.spawn () ~prog ~argv ~stdout:w |> Pid.of_int in
    Unix.close w;
    (r, pid)

  let create q =
    let files_changed = ref [] in
    let event_mtx = Mutex.create () in
    let event_cv = Condition.create () in
    let worker_thread pipe =
      let buffer = { data = Bytes.create buffer_capacity; size = 0 } in
      while true do
        let lines = List.map (read_lines buffer pipe) ~f:Path.of_string in
        Mutex.lock event_mtx;
        files_changed := List.rev_append lines !files_changed;
        Condition.signal event_cv;
        Mutex.unlock event_mtx
      done
    in
    (* The buffer thread is used to avoid flooding the main thread with file
       changes events when a lot of file changes are reported at once. In
       particular, this avoids restarting the build over and over in a short
       period of time when many events are reported at once.

       It works as follow:

       - when the first event is received, send it to the main thread
       immediately so that we get a fast response time

       - after the first event is received, buffer subsequent events for
       [buffering_time] *)
    let rec buffer_thread () =
      Mutex.lock event_mtx;
      while List.is_empty !files_changed do
        Condition.wait event_cv event_mtx
      done;
      let files = !files_changed in
      files_changed := [];
      Mutex.unlock event_mtx;
      Event.Queue.send_files_changed q files;
      Thread.delay buffering_time;
      buffer_thread ()
    in
    let pipe, pid = spawn_external_watcher () in
    ignore (Thread.create worker_thread pipe : Thread.t);
    ignore (Thread.create buffer_thread () : Thread.t);
    pid
end

module Process_watcher : sig
  (** Initialize the process watcher thread. *)
  type t

  val init : Event.Queue.t -> t

  (** Register a new running job. *)
  val register_job : t -> job -> unit

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
    Process_table.iter t ~f:(fun job ->
        try Unix.kill (Pid.to_int job.pid) signal with
        | Unix.Unix_error _ -> ());
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
    with
    | Finished proc_info ->
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

  let wait =
    if Sys.win32 then
      wait_win32
    else
      wait_unix

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
    ignore (Thread.create run t : Thread.t);
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
      let r, w = Unix.pipe () in
      let buf = Bytes.create 1 in
      Sys.set_signal Sys.sigint
        (Signal_handle (fun _ -> assert (Unix.write w buf 0 1 = 1)));
      Staged.stage (fun () ->
          assert (Unix.read r buf 0 1 = 1);
          Signal.Int)
    ) else
      Staged.stage (fun () ->
          Thread.wait_signal signos |> Signal.of_int |> Option.value_exn)

  let run q =
    let last_exit_signals = Queue.create () in
    let wait_signal = Staged.unstage (signal_waiter ()) in
    while true do
      let signal = wait_signal () in
      Event.Queue.send_signal q signal;
      match signal with
      | Int
      | Quit
      | Term ->
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

  let init q = ignore (Thread.create run q : Thread.t)
end

type waiting_for_file_changes =
  | Shutdown_requested
  | File_system_changed

type status =
  (* Waiting for file changes to start a new a build *)
  | Waiting_for_file_changes of waiting_for_file_changes Fiber.Ivar.t
  (* Running a build *)
  | Building
  (* Cancellation requested. Build jobs are immediately rejected in this state *)
  | Restarting_build
  (* Shut down requested. No new new builds will start *)
  | Shutting_down

module Handler = struct
  module Event = struct
    type build_result =
      | Success
      | Failure

    type t =
      | Tick
      | Source_files_changed
      | Build_interrupted
      | Build_finish of build_result
  end

  type t = Config.t -> Event.t -> unit
end

type t =
  { config : Config.t
  ; mutable status : status
  ; handler : Handler.t
  ; job_throttle : Fiber.Throttle.t
  ; events : Event.Queue.t
  ; process_watcher : Process_watcher.t
  ; csexp_scheduler : Csexp_rpc.Scheduler.t Lazy.t
  }

let t : t Fiber.Var.t = Fiber.Var.create ()

let set x f = Fiber.Var.set t x f

let t () = Fiber.Var.get_exn t

let running_jobs_count t = Event.Queue.pending_jobs t.events

let ignore_for_watch p =
  let+ t = t () in
  Event.Queue.ignore_next_file_change_event t.events p

exception Cancel_build

let with_job_slot f =
  let* t = t () in
  Fiber.Throttle.run t.job_throttle ~f:(fun () ->
      match t.status with
      | Restarting_build
      | Shutting_down ->
        raise Cancel_build
      | Building -> f t.config
      | Waiting_for_file_changes _ ->
        (* At this stage, we're not running a build, so we shouldn't be running
           tasks here. *)
        assert false)

(* We use this version privately in this module whenever we can pass the
   scheduler explicitly *)
let wait_for_process t pid =
  let ivar = Fiber.Ivar.create () in
  Process_watcher.register_job t.process_watcher { pid; ivar };
  Fiber.Ivar.read ivar

let global = ref None

let wait_for_dune_cache () =
  let t = Option.value_exn !global in
  Event.Queue.flush_sync_tasks t.events

let got_signal signal =
  if !Log.verbose then
    Log.info [ Pp.textf "Got signal %s, exiting." (Signal.name signal) ]

type saw_signal =
  | Ok
  | Got_signal

let kill_and_wait_for_all_processes t =
  Process_watcher.killall t.process_watcher Sys.sigkill;
  let saw_signal = ref Ok in
  while Event.Queue.pending_jobs t.events > 0 do
    match Event.Queue.next t.events with
    | Signal signal ->
      got_signal signal;
      saw_signal := Got_signal
    | _ -> ()
  done;
  !saw_signal

let prepare (config : Config.t) ~(handler : Handler.t) =
  let events = Event.Queue.create config.stats in
  (* The signal watcher must be initialized first so that signals are blocked in
     all threads. *)
  Signal_watcher.init events;
  let process_watcher = Process_watcher.init events in
  let csexp_scheduler =
    lazy
      (let create_thread_safe_ivar () =
         let ivar = Fiber.Ivar.create () in
         let fill v =
           Event.Queue.send_rpc_completed events (Fiber.Fill (ivar, v))
         in
         Event.Queue.register_rpc_started events;
         (ivar, fill)
       in
       { Csexp_rpc.Scheduler.create_thread_safe_ivar
       ; spawn_thread = Thread.spawn
       })
  in
  let t =
    { status = Building
    ; job_throttle = Fiber.Throttle.create config.concurrency
    ; process_watcher
    ; events
    ; config
    ; handler
    ; csexp_scheduler
    }
  in
  global := Some t;
  t

module Run_once : sig
  type run_error =
    | Got_signal
    | Never
    | Exn of Exn_with_backtrace.t

  (** Run the build and clean up after it (kill any stray processes etc). *)
  val run_and_cleanup : t -> (unit -> 'a Fiber.t) -> ('a, run_error) Result.t
end = struct
  type run_error =
    | Got_signal
    | Never
    | Exn of Exn_with_backtrace.t

  exception Abort of run_error

  (** This function is the heart of the scheduler. It makes progress in
      executing fibers by doing the following:

      - notifying completed jobs
      - starting cancellations
      - terminating the scheduler on signals *)
  let rec iter (t : t) =
    if
      (match t.status with
      | Waiting_for_file_changes _ ->
        (* In polling mode, there are no pending jobs while we are waiting for
           file changes *)
        false
      | _ -> true)
      && Event.Queue.pending_jobs t.events = 0
      && Event.Queue.pending_rpc t.events = 0
    then
      raise (Abort Never)
    else (
      t.handler t.config Tick;
      match Event.Queue.next t.events with
      | Job_completed (job, proc_info) -> Fiber.Fill (job.ivar, proc_info)
      | File_system_changed events -> (
        match (Fs_memo.handle events : Fs_memo.Rebuild_required.t) with
        | No -> iter t (* Ignore the event *)
        | Yes -> (
          Memo.reset ();
          match t.status with
          | Shutting_down
          | Restarting_build ->
            (* We're already cancelling build, so file change events don't
               matter *)
            iter t
          | Building ->
            t.handler t.config Build_interrupted;
            t.status <- Restarting_build;
            Process_watcher.killall t.process_watcher Sys.sigkill;
            iter t
          | Waiting_for_file_changes ivar -> Fill (ivar, File_system_changed)))
      | Rpc fill -> fill
      | Signal signal ->
        got_signal signal;
        raise (Abort Got_signal)
    )

  let run t f : _ result =
    let fiber = set t (fun () -> Fiber.with_error_handler f ~on_error) in
    match Fiber.run fiber ~iter:(fun () -> iter t) with
    | res ->
      assert (Event.Queue.pending_jobs t.events = 0);
      assert (Event.Queue.pending_rpc t.events = 0);
      Ok res
    | exception Abort err -> Error err
    | exception exn -> Error (Exn (Exn_with_backtrace.capture exn))

  let run_and_cleanup t f =
    let res = run t f in
    Console.Status_line.set (Fun.const None);
    match kill_and_wait_for_all_processes t with
    | Got_signal -> Error Got_signal
    | Ok -> res
end

module Run = struct
  type file_watcher =
    | Detect_external
    | No_watcher

  module Event = Handler.Event

  let poll step =
    let* t = t () in
    let rec loop () : unit Fiber.t =
      t.status <- Building;
      let* res =
        let on_error exn =
          (match t.status with
          | Building -> Report_error.report exn
          | Shutting_down
          | Restarting_build ->
            ()
          | Waiting_for_file_changes _ ->
            (* We are inside a build, so we aren't waiting for a file change
               event *)
            assert false);
          Fiber.return ()
        in
        Fiber.map_reduce_errors (module Monoid.Unit) ~on_error step
      in
      match t.status with
      | Waiting_for_file_changes _ ->
        (* We just finished a build, so there's no way this was set *)
        assert false
      | Shutting_down -> Fiber.return ()
      | Restarting_build -> loop ()
      | Building -> (
        t.handler t.config
          (Build_finish
             (match res with
             | Error _ -> Failure
             | Ok _ -> Success));
        let ivar = Fiber.Ivar.create () in
        t.status <- Waiting_for_file_changes ivar;
        let* next = Fiber.Ivar.read ivar in
        match next with
        | Shutdown_requested -> Fiber.return ()
        | File_system_changed -> (
          t.handler t.config Source_files_changed;
          match res with
          | Error _
          | Ok `Continue ->
            loop ()
          | Ok `Stop -> Fiber.return ()))
    in
    loop ()

  exception Shutdown_requested

  let go config ?(file_watcher = No_watcher)
      ~(on_event : Config.t -> Handler.Event.t -> unit) run =
    let t = prepare config ~handler:on_event in
    let watcher =
      match file_watcher with
      | No_watcher -> None
      | Detect_external -> Some (File_watcher.create t.events)
    in
    let result =
      match Run_once.run_and_cleanup t run with
      | Ok a -> Result.Ok a
      | Error (Got_signal | Never) ->
        let exn =
          if t.status = Shutting_down then
            Shutdown_requested
          else
            Dune_util.Report_error.Already_reported
        in
        Error (exn, None)
      | Error (Exn exn_with_bt) ->
        Error (exn_with_bt.exn, Some exn_with_bt.backtrace)
    in
    Option.iter watcher ~f:(fun watcher ->
        ignore (wait_for_process t (File_watcher.pid watcher) : _ Fiber.t));
    ignore (kill_and_wait_for_all_processes t : saw_signal);
    match result with
    | Ok a -> a
    | Error (exn, None) -> Exn.raise exn
    | Error (exn, Some bt) -> Exn.raise_with_backtrace exn bt
end

let send_sync_task d =
  let t = Option.value_exn !global in
  Event.Queue.send_sync_task t.events d

let wait_for_process pid =
  let* t = t () in
  wait_for_process t pid

let shutdown () =
  let* t = t () in
  let fill_file_changes =
    match t.status with
    | Waiting_for_file_changes ivar -> Fiber.Ivar.fill ivar Shutdown_requested
    | _ -> Fiber.return ()
  in
  t.status <- Shutting_down;
  fill_file_changes

let csexp_scheduler () =
  let+ t = t () in
  Lazy.force t.csexp_scheduler
