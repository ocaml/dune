open Stdune

type action =
  | Continue
  | Reap_processes
  | Shutdown of Shutdown.Reason.t

val with_ : Event.Queue.t -> f:(unit -> 'a) -> 'a
val handle : Signal.t -> action
val is_exit_signal : Signal.t -> bool
val print_ctrl_c_warning : unit -> unit
val emergency_exit : unit -> 'a
