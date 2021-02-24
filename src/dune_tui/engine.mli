(** Text user interface engine *)

type t

(** Start the text ui engine. [main] is a function that will be called in a
    separate thread and implementes the UI logic. *)
val start : main:(t -> unit) -> spawn_thread:((unit -> unit) -> unit) -> unit

(** Terminal size *)
val size : t -> int * int

(** Update the screen *)
val update : t -> screen:Notty.image -> cursor:(int * int) option -> unit

module Event : sig
  type t =
    | Input of Notty.Unescape.event
    | Resize of int * int
end

val next : t -> Event.t

(** Indicate that a [SIGWINCH] event was received *)
val send_winch : t -> unit
