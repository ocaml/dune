(** Preprocessing of OCaml source files *)

open Import

val pped_modules_map :
     Preprocess.Without_instrumentation.t Preprocess.t Module_name.Per_item.t
  -> Ocaml.Version.t
  -> (Module.t -> Module.t) Staged.t

val make :
     Super_context.t
  -> dir:Path.Build.t
  -> expander:Expander.t
  -> lint:Preprocess.Without_instrumentation.t Preprocess.Per_module.t
  -> preprocess:Preprocess.Without_instrumentation.t Preprocess.Per_module.t
  -> preprocessor_deps:Dep_conf.t list
  -> instrumentation_deps:Dep_conf.t list
  -> lib_name:Lib_name.Local.t option
  -> Pp_spec.t

val gen_rules : Super_context.t -> string list -> unit Memo.t

val action_for_pp_with_target :
     dir:Path.Build.t
  -> sandbox:Sandbox_config.t
  -> loc:Loc.t
  -> expander:Expander.t
  -> action:Action_unexpanded.t
  -> src:Path.Build.t
  -> target:Path.Build.t
  -> lib_name:Lib_name.Local.t option
  -> ml_kind:Ml_kind.t
  -> Action.Full.t Action_builder.With_targets.t Memo.t

val ppx_exe :
  Context.t -> scope:Scope.t -> Lib_name.t -> Path.Build.t Resolve.Memo.t
