open! Stdune

type 'a gen = {
  none : 'a;
  symlink : 'a;
  copy : 'a;
}

(** A set of sandbox modes in which the rule is expected
    to work correctly. *)
type t = bool gen

val compare : t -> t -> Ordering.t

val equal : t -> t -> bool

(** Computes the intersection of allowed sandbox modes *)
val inter : t -> t -> t

val no_special_requirements : t

val no_sandboxing : t

val needs_sandboxing : t

(** The default sandboxing config for actions that don't bother specifying it.

    Often this means that they don't have special requirements, but it also
    often means that we're not quite sure.

    Currently we have [default = no_special_requirements].
*)
val default : t

(** The default sandboxing config for user rules.

    We currently assume that user rules must not be sandboxed, but that's a
    terrible assumption.
*)
val user_rule : t

val disallow : Sandbox_mode.t -> t

val mem : t -> Sandbox_mode.t -> bool

module Partial : sig
  type t = bool option gen

  (** [merge] distributes across [inter] when there is no error, but it can
      detect a nonsensical configuration where [inter] can't.

      Can raise a User_error.
  *)
  val merge : loc:Loc.t -> t list -> bool gen

  val no_special_requirements : t

  val no_sandboxing : t

  val needs_sandboxing : t

  val disallow : Sandbox_mode.t -> t

end
