open! Stdune

(** A function [Sandbox_mode.t -> bool] returning true if the rule is expected
    to work correctly (respecting its specified dependencies) in this mode. *)

type t = {
  none : bool;
  symlink : bool;
  copy : bool;
}

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
