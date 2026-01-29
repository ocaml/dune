open Import

(** Combine a list of actions into a single optional action. *)
val make_action : Dune_lang.Action.t list -> Dune_lang.Action.t option

(** Build environment from opam file. *)
val build_env : OpamFile.OPAM.t -> String_with_vars.t Dune_lang.Action.Env_update.t list

(** Wrap an action with the build environment from an opam file. *)
val wrap_build_env : OpamFile.OPAM.t -> Dune_lang.Action.t -> Dune_lang.Action.t
