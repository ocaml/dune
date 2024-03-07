(** An exception together with the backtrace that raised it. *)

type t =
  { exn : exn
  ; backtrace : Printexc.raw_backtrace
  }

val try_with : (unit -> 'a) -> ('a, t) Result.t
val try_with_never_returns : (unit -> Nothing.t) -> t

(** This function should be the very first thing called in the exception handler
    if you want it to work correctly. Otherwise it might capture an incorrect
    backtrace. *)
val capture : exn -> t

val reraise : t -> 'a
val pp_uncaught : Format.formatter -> t -> unit
val pp : t -> _ Pp.t
val map : t -> f:(exn -> exn) -> t
val map_and_reraise : t -> f:(exn -> exn) -> 'a
val to_dyn : t -> Dyn.t
