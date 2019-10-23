open Stdune

(** Represent a run of the system *)
type t

val to_dyn : t -> Dyn.t

(** Return the current run *)
val current : unit -> t

(** Whether this run is the current one *)
val is_current : t -> bool

(** End the current run and start a new one *)
val restart : unit -> unit
