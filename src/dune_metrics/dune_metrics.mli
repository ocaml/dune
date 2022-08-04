open Stdune

(** Utilities for collecting performance metrics *)

(** This function must be called to enable all performance metrics. *)
val enable : unit -> unit

(** Reset all metrics to zero. *)
val reset : unit -> unit

module Timer : sig
  module Measure : sig
    type t =
      { cumulative_time : float
      ; count : int
      }
  end

  type t

  val start : string -> t

  val stop : t -> unit

  (** If metrics are enabled, increment the timer by the amount of seconds
      elapsed during the execution of [f]. *)
  val record : string -> f:(unit -> 'a) -> 'a

  val aggregated_timers : unit -> Measure.t String.Map.t
end
