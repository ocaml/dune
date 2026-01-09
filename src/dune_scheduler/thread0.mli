open Stdune

type t = Thread.t

val interrupt_signals : Signal.t list
val spawn : (unit -> unit) -> Thread.t
val delay : float -> unit
val wait_signal : int list -> int
