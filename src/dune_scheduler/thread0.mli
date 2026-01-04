open Stdune

val interrupt_signals : Signal.t list
val spawn : (unit -> unit) -> unit
val delay : float -> unit
val wait_signal : int list -> int
