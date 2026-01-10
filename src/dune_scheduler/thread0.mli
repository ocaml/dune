open Stdune

type t = Thread.t

(** Magic signal to interrupt to the signal watching thread *)
val signal_watcher_interrupt : Signal.t

val join : t -> unit
val interrupt_signals : Signal.t list
val spawn : (unit -> unit) -> Thread.t
val delay : float -> unit
val wait_signal : int list -> int
