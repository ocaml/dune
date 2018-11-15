(** Collect stats during the execution of dune *)

(** Enable stats recording *)
val enable : unit -> unit

(** If stats recording is enabled, collect stats now *)
val record : unit -> unit
