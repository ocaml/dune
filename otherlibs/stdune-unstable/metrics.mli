(** Utilities for collecting performance metrics *)

(** This function must be called to enable all performance metrics. *)
val enable : unit -> unit

(** Reset all metrics to zero. *)
val reset : unit -> unit

module Timer : sig
  type t

  (* Create a timer initialised to 0 and hooked to the global [reset]. *)
  val create : unit -> t

  val read_seconds : t -> float

  (** If metrics are enabled, increment the timer by the amount of seconds
      elapsed during the execution of [f]. *)
  val record : t -> f:(unit -> 'a) -> 'a
end
