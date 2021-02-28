open Stdune

module Event : sig
  (** Abstract events emitted by the UI *)

  type t = Quit
end

type t

val create : on_event:(Event.t -> unit) -> t

val backend : t -> (module Console.Backend.S)
