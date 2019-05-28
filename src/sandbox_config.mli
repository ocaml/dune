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
    often means that we're not sure and there might be some requirements
    that we didn't yet discover because we never tried sandboxing it.

    Currently we have [default = no_sandboxing] to be consistent with the old
    dune behavior, but we'd like to change it to
    [default = no_special_requirements].
*)
val default : t
