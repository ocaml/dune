(** This module deals with management of hooks that run after specific events
    (e.g. end of build). *)

module type S = sig
  type 'a t

  (** Register a hook called every time the event occurs. *)
  val always : (unit -> unit t) -> unit

  (** Signalize the event and run all registered hooks. *)
  val run : unit -> unit t
end

(** Every time a build starts, which includes every iteration in watch mode. These hooks
    are executed in parallel. *)
module Start_of_build : S with type 'a t := 'a Action_builder0.t

(** Every time a build successfully ends, which includes every iteration in watch mode.
    These hooks are executed in parallel. *)
module End_of_build : S with type 'a t := 'a Action_builder0.t

(** Hooks run after each build (including incremental), whether it ends succesfully or not. *)
module Post_build : S with type 'a t := 'a
