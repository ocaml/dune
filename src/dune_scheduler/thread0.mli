open Stdune

type t = Thread.t

(** Signal that asks dune to emit debugging info. *)
val debug_signal : Signal.t

val join : t -> unit
val interrupt_signals : Signal.t list
val spawn : name:string -> (unit -> unit) -> Thread.t
val delay : float -> unit
