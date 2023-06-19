open Stdune

(** The signals that must be handled in the thread that's handling terminal ui
    events. *)
val signals : Signal.t list

val unblock : unit -> unit
