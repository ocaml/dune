(** Actions as they are written in dune files. *)

open Import

include module type of struct
  (** The type definition exists in [Action_dune_lang] and not here to break
      cycles.*)
  include Dune_lang.Action
end

val remove_locs : t -> t

(** Expand an action and return its target and dependencies.

    Expanding an action substitutes all [%{..}] forms, discovers dependencies
    and targets, and verifies invariants such as:

    - All the targets are in [targets_dir]
    - The [targets] mode is respected *)
val expand
  :  t
  -> loc:Loc.t
  -> chdir:Path.Build.t
  -> deps:Dep_conf.t Bindings.t
  -> targets_dir:Path.Build.t
  -> targets:Path.Build.t Targets_spec.t
  -> expander:Expander.t
  -> Action.Full.t Action_builder.With_targets.t Memo.t

(** [what] as the same meaning as the argument of
    [Expander.Expanding_what.User_action_without_targets] *)
val expand_no_targets
  :  t
  -> loc:Loc.t
  -> chdir:Path.Build.t
  -> deps:Dep_conf.t Bindings.t
  -> expander:Expander.t
  -> what:string
  -> Action.Full.t Action_builder.t
