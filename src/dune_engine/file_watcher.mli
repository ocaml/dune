open! Stdune
open Import

type t

(** Create a new file watcher. *)
val create : thread_safe_send_files_changed:(Path.t list -> unit) -> t

(** Pid of the external file watcher process *)
val pid : t -> Pid.t
