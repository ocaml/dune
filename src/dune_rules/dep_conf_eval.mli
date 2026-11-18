(** Interpret dependencies written in Dune files *)

open Import

(** Evaluates unnamed dependency specifications. The returned builder both
    registers the rule's dependencies (as a side effect of evaluation) and
    produces an [Env.t] augmentation that callers should fold into the action
    via [Action.Full.add_env]. For build-tracking-only call sites (no action
    constructed), evaluate the builder and discard the env via
    [Action_builder.ignore]. *)
val unnamed
  :  Sandbox_config.t
  -> expander:Expander.t
  -> Dep_conf.t list
  -> Env.t Action_builder.t * Sandbox_config.t

(** Evaluates unnamed dependency specifications. Returns the paths to the newly
    evaluated dependencies. *)
val unnamed_get_paths
  :  expander:Expander.t
  -> Dep_conf.t list
  -> Path.Set.t Action_builder.t * Sandbox_config.t option

(** Evaluates named dependency specifications. The returned builder both
    registers the rule's dependencies and produces an [Env.t] augmentation,
    same convention as [unnamed]. The expander has the named bindings added
    so [%{name}] references resolve. *)
val named
  :  Sandbox_config.t
  -> expander:Expander.t
  -> Dep_conf.t Bindings.t
  -> Env.t Action_builder.t * Expander.t * Sandbox_config.t Action_builder.t
