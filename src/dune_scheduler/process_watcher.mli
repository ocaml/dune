open Stdune

val kill_process_group : Pid.t -> int -> unit

(** Initialize the process watcher thread. *)
type t

val init : Event.Queue.t -> t

(** Register a new running job. *)
val register_job : t -> Event.job -> unit

val is_running : t -> Pid.t -> bool

(** Send the following signal to all running processes. *)
val killall : t -> int -> unit
