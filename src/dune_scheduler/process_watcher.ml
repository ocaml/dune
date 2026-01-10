open Stdune

let kill_process_group pid signal =
  match Sys.win32 with
  | false ->
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
    (try Unix.kill (-Pid.to_int pid) signal with
     (* CR-someday rgrinerg: do we need to catch this error now that we're no
        longer racing? *)
     | Unix.Unix_error _ -> ())
  | true ->
    (* Process groups are not supported on Windows (or even if they are, [spawn]
       does not know how to use them), so we're only sending the signal to the
       job itself. *)
    (try Unix.kill (Pid.to_int pid) signal with
     | Unix.Unix_error _ -> ())
;;

type process_state =
  | Running of Event.job
  | Zombie of Proc.Process_info.t

(* This mutable table is safe: it does not interact with the state we track in
     the build system. *)
type t =
  { mutex : Mutex.t Lazy.t
  ; something_is_running : Condition.t option
  ; table : (Pid.t, process_state) Table.t
  ; events : Event.Queue.t
  ; mutable running_count : int
  }

let is_running_unix t pid = Table.mem t.table pid

let is_running_win32 t pid =
  let mutex = Lazy.force t.mutex in
  Mutex.lock mutex;
  let res = is_running_unix t pid in
  Mutex.unlock mutex;
  res
;;

let is_running = if Sys.win32 then is_running_win32 else is_running_unix

module Process_table : sig
  val add : t -> Event.job -> unit
  val remove : t -> Proc.Process_info.t -> Event.job option
  val running_count : t -> int
  val iter : t -> f:(Event.job -> unit) -> unit
end = struct
  let add t (job : Event.job) =
    match Table.find t.table job.pid with
    | None ->
      Table.set t.table job.pid (Running job);
      t.running_count <- t.running_count + 1;
      (match t.something_is_running with
       | None -> ()
       | Some something_is_running ->
         if t.running_count = 1 then Condition.signal something_is_running)
    | Some (Zombie proc_info) ->
      Table.remove t.table job.pid;
      Event.Queue.send_job_completed t.events job proc_info
    | Some (Running _) -> assert false
  ;;

  let remove t (proc_info : Proc.Process_info.t) =
    match Table.find t.table proc_info.pid with
    | None ->
      Table.set t.table proc_info.pid (Zombie proc_info);
      None
    | Some (Running job) ->
      t.running_count <- t.running_count - 1;
      Table.remove t.table proc_info.pid;
      Some job
    | Some (Zombie _) -> assert false
  ;;

  let iter t ~f =
    Table.iter t.table ~f:(fun data ->
      match data with
      | Running job -> f job
      | Zombie _ -> ())
  ;;

  let running_count t = t.running_count
end

let register_job_win32 t job =
  Event.Queue.register_job_started t.events;
  let mutex = Lazy.force t.mutex in
  Mutex.lock mutex;
  Process_table.add t job;
  Mutex.unlock mutex
;;

let register_job_unix t job =
  Event.Queue.register_job_started t.events;
  Process_table.add t job
;;

let register_job = if Sys.win32 then register_job_win32 else register_job_unix

let killall_unix t signal =
  Process_table.iter t ~f:(fun job -> kill_process_group job.pid signal)
;;

let killall_win32 t signal =
  let mutex = Lazy.force t.mutex in
  Mutex.lock mutex;
  killall_unix t signal;
  Mutex.unlock mutex
;;

let killall = if Sys.win32 then killall_win32 else killall_unix

exception Finished of Proc.Process_info.t

let wait_nonblocking_win32 t =
  try
    Process_table.iter t ~f:(fun job ->
      let pid, status = Unix.waitpid [ WNOHANG ] (Pid.to_int job.pid) in
      if pid <> 0
      then (
        let now = Time.now () in
        let info : Proc.Process_info.t =
          { pid = Pid.of_int pid; status; end_time = now; resource_usage = None }
        in
        raise_notrace (Finished info)));
    false
  with
  | Finished proc_info ->
    Process_table.remove t proc_info
    |> Option.iter ~f:(fun job -> Event.Queue.send_job_completed t.events job proc_info);
    true
;;

let rec wait_unix t acc =
  match Proc.wait Any [ WNOHANG ] with
  | None -> acc
  | Some proc_info ->
    (match Process_table.remove t proc_info with
     | None ->
       Dune_trace.emit Process (fun () -> Dune_trace.Event.unknown_process proc_info);
       wait_unix t acc
     | Some job ->
       Event.Queue.finish_job t.events;
       let acc = Fiber.Fill (job.ivar, proc_info) :: acc in
       wait_unix t acc)
;;

let wait_unix t = List.rev (wait_unix t [])

let run_win32 t =
  let mutex = Lazy.force t.mutex in
  let something_is_running = Option.value_exn t.something_is_running in
  Mutex.lock mutex;
  while true do
    while Process_table.running_count t = 0 do
      Condition.wait something_is_running mutex
    done;
    while not (wait_nonblocking_win32 t) do
      Mutex.unlock mutex;
      Thread.delay 0.001;
      Mutex.lock mutex
    done
  done
;;

let init events =
  let t =
    { mutex =
        lazy
          (if Sys.win32
           then Mutex.create ()
           else Code_error.raise "process watcher mutex is only needed on windows" [])
    ; something_is_running = (if Sys.win32 then Some (Condition.create ()) else None)
    ; table = Table.create (module Pid) 128
    ; events
    ; running_count = 0
    }
  in
  if Sys.win32
  then (
    let (_ : Thread.t) = Thread0.spawn (fun () -> run_win32 t) in
    ());
  t
;;
