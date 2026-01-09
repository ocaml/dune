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
;;

module Process_table : sig
  val add : t -> Event.job -> unit
  val remove : t -> Proc.Process_info.t -> unit
  val running_count : t -> int
  val iter : t -> f:(Event.job -> unit) -> unit
end = struct
  let add t (job : Event.job) =
    match Table.find t.table job.pid with
    | None ->
      Table.set t.table job.pid (Running job);
      t.running_count <- t.running_count + 1;
      if t.running_count = 1 then Condition.signal t.something_is_running
    | Some (Zombie proc_info) ->
      Table.remove t.table job.pid;
      Event.Queue.send_job_completed t.events job proc_info
    | Some (Running _) -> assert false
  ;;

  let remove t (proc_info : Proc.Process_info.t) =
    match Table.find t.table proc_info.pid with
    | None -> Table.set t.table proc_info.pid (Zombie proc_info)
    | Some (Running job) ->
      t.running_count <- t.running_count - 1;
      Table.remove t.table proc_info.pid;
      Event.Queue.send_job_completed t.events job proc_info
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

let register_job t job =
  Event.Queue.register_job_started t.events;
  Mutex.lock t.mutex;
  Process_table.add t job;
  Mutex.unlock t.mutex
;;

let killall t signal =
  Mutex.lock t.mutex;
  Process_table.iter t ~f:(fun job -> kill_process_group job.pid signal);
  Mutex.unlock t.mutex
;;

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
    (* We need to do the [Unix.waitpid] and remove the process while holding
         the lock, otherwise the pid might be reused in between. *)
    Process_table.remove t proc_info;
    true
;;

let wait_win32 t =
  while not (wait_nonblocking_win32 t) do
    Mutex.unlock t.mutex;
    Thread.delay 0.001;
    Mutex.lock t.mutex
  done
;;

let wait_unix t =
  Mutex.unlock t.mutex;
  let proc_info = Proc.wait Any [] in
  Mutex.lock t.mutex;
  Process_table.remove t proc_info
;;

let wait =
  match Platform.OS.value with
  | Windows -> wait_win32
  | Linux | Darwin | FreeBSD | OpenBSD | NetBSD | Haiku | Other -> wait_unix
;;

let run t =
  Mutex.lock t.mutex;
  while true do
    while Process_table.running_count t = 0 do
      Condition.wait t.something_is_running t.mutex
    done;
    wait t
  done
;;

let init events =
  let t =
    { mutex = Mutex.create ()
    ; something_is_running = Condition.create ()
    ; table = Table.create (module Pid) 128
    ; events
    ; running_count = 0
    }
  in
  let (_ : Thread.t) = Thread0.spawn (fun () -> run t) in
  t
;;
