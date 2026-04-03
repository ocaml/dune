open Stdune

type t = Thread.t

(** Magic signal to interrupt to the signal watching thread *)
val signal_watcher_interrupt : Signal.t

(** Magic signal to make dune debugging info *)
val signal_watcher_debug : Signal.t

val interrupt_signals : Signal.t list
val join : t -> unit
val wait_signal : int list -> int
val spawn : name:string -> (unit -> unit) -> Thread.t
val delay : float -> unit
