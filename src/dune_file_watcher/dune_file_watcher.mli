open! Stdune

type t

(** Create a new file watcher.
   [debounce_interval] is measured in seconds and it controls the minimum
*)
val create :
  root:Path.t
  -> debounce_interval:float option
  -> thread_safe_send_files_changed:(Path.t list -> unit)
  -> t

(** Create a new file watcher with default settings. *)
val create_default :
  thread_safe_send_files_changed:(Path.t list -> unit)
  -> t

(** Pid of the external file watcher process *)
val pid : t -> Pid.t

val wait_watches_established : t -> unit
