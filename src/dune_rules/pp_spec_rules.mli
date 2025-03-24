(** Preprocessing of OCaml source files *)

open Import

val make
  :  Super_context.t
  -> dir:Path.Build.t
  -> expander:Expander.t
  -> lint:Preprocess.Without_instrumentation.t Preprocess.Per_module.t
  -> preprocess:Preprocess.Without_instrumentation.t Preprocess.Per_module.t
  -> preprocessor_deps:Dep_conf.t list
  -> instrumentation_deps:Dep_conf.t list
  -> lib_name:Lib_name.Local.t option
  -> scope:Scope.t
  -> Pp_spec.t Memo.t

val gen_rules : Super_context.t -> string list -> unit Memo.t

val action_for_pp_with_target
  :  sandbox:Sandbox_config.t
  -> loc:Loc.t
  -> expander:Expander.t
  -> action:Action_unexpanded.t
  -> src:Path.Build.t
  -> target:Path.Build.t
  -> Action.Full.t Action_builder.With_targets.t
