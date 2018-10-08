(** Collect stats during the execution of dune *)

(** Enable stats recording *)
val enable : unit -> unit

(** If stats recording is enabled, collect stats now *)
val record : unit -> unit

module Catapult : sig
  (** Output trace data to a file in catapult format.
      This format is compatible with [chrome://tracing]. *)

  val enable : string -> unit

  type event

  val on_process_start : program:string -> args:string list -> event

  val on_process_end : event -> unit
end
