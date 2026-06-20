open Stdune

val kill_process_group : Pid.t -> Signal.t -> is_process_group_leader:bool -> unit

(** Initialize the process watcher thread. *)
type t

val to_dyn : t -> Dyn.t
val init : Event.Queue.t -> t

(** Register a new running job. *)
val register_job : t -> Event.job -> unit

val is_running : t -> Pid.t -> bool
val running_count : t -> int

(** Send the following signal to all running processes. *)
val killall : t -> Signal.t -> unit

val wait_unix : t -> Fiber.fill list
