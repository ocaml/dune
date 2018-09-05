(** This module deals with management of hooks that run
    after specific events (e.g. end of build). *)

module End_of_build : sig
  (** Register a hook called at the end of every build.

      For watch mode, this means that once registered, the hook
      will be called after every iteration. *)
  val always : (unit -> unit) -> unit

  (** Register a hook called at the end of current build only.

      For watch mode, this means that after current iteration
      is over, the hook will be called and deregistered
      automatically. *)
  val once : (unit -> unit) -> unit


  (** Signalize end of build and run all registered hooks. *)
  val run : unit -> unit
end
