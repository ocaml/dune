(** Sandboxing configuration of build rules *)

(** This module manages the sandboxing configuration written by the user in dune
    files or inside the action builder.

    The sandboxing configuration of a build rule represent what the rule expects
    in terms of sandboxing. For instance, a rule might not work correctly when
    it is not sandboxed, or the opposite. *)

open Import

(** A set of sandbox modes in which the rule is expected to work correctly. *)
type t = Sandbox_mode.Set.t

val compare : t -> t -> Ordering.t
val equal : t -> t -> bool

(** Computes the intersection of allowed sandbox modes *)
val inter : t -> t -> t

val no_special_requirements : t
val no_sandboxing : t

(** Allow any sandboxing mode, except [Patch_back_source_tree] *)
val needs_sandboxing : t

(** The default sandboxing config for actions that don't bother specifying it.

    Often this means that they don't have special requirements, but it also
    often means that we're not quite sure.

    Currently we have [default = no_special_requirements]. *)
val default : t

val disallow : Sandbox_mode.t -> t
val mem : t -> Sandbox_mode.t -> bool

module Partial : sig
  type t = bool option Sandbox_mode.Dict.t

  (** [merge] distributes across [inter] when there is no error, but it can
      detect a nonsensical configuration where [inter] can't.

      Can raise a User_error. *)
  val merge : loc:Loc.t -> t list -> Sandbox_mode.Set.t

  val no_special_requirements : t
  val no_sandboxing : t
  val needs_sandboxing : t
  val disallow : Sandbox_mode.t -> t
end
