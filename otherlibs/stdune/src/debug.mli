(** Register debug information dumpers that can be triggered with [Sigusr1] *)
val register : name:string -> (unit -> Dyn.t) -> unit

val dump : unit -> (string * Dyn.t) list
