(** This module deals with management of hooks that run after specific events
    (e.g. end of build). *)

module type S = sig
  val always : (unit -> unit) -> unit
  (** Register a hook called every time the event occurs. *)

  val once : (unit -> unit) -> unit
  (** Register a hook that will only be called once when the next event occurs. *)

  val run : unit -> unit
  (** Signalize the event and run all registered hooks. *)
end

module Make () : S

(** Every time a build ends, which includes every iteration in watch mode,
    including cancellation of build because of file changes. *)
module End_of_build : S
