open Stdune

(** Abstract events emitted by the UI that must be handled separately. *)
type event = Quit

type t

val create : on_event:(event -> unit) -> t

val backend : t -> (module Console.Backend.S)
