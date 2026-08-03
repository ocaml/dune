(** The signals that must be handled in the thread that's handling terminal ui
    events. *)
val signals : Signal.t list

type mask

val block : unit -> mask
val restore : mask -> unit
val unblock : unit -> unit
