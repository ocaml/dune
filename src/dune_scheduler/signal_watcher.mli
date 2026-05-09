open Stdune

type action =
  | Continue
  | Reap_processes
  | Shutdown of Shutdown.Reason.t

val init : print_ctrl_c_warning:bool -> Event.Queue.t -> unit
val cleanup : Event.Queue.t -> unit
val handle : Signal.t -> action
