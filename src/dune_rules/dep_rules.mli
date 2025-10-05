(** Get dependencies for a set of modules using either ocamldep or ocamlobjinfo *)

open Import

val for_module
  :  obj_dir:Path.Build.t Obj_dir.t
  -> modules:Modules.With_vlib.t
  -> sandbox:Sandbox_config.t
  -> impl:Virtual_rules.t
  -> dir:Path.Build.t
  -> sctx:Super_context.t
  -> Module.t
  -> Module.t list Action_builder.t Ml_kind.Dict.t Memo.t

val immediate_deps_of
  :  Module.t
  -> Modules.With_vlib.t
  -> obj_dir:Path.Build.t Obj_dir.t
  -> ml_kind:Ml_kind.t
  -> Module.t list Action_builder.t

val rules
  :  obj_dir:Path.Build.t Obj_dir.t
  -> modules:Modules.With_vlib.t
  -> sandbox:Sandbox_config.t
  -> impl:Virtual_rules.t
  -> sctx:Super_context.t
  -> dir:Path.Build.t
  -> Dep_graph.Ml_kind.t Memo.t

val deps_of
  :  obj_dir:Path.Build.t Obj_dir.t
  -> modules:Modules.With_vlib.t
  -> sandbox:Sandbox_config.t
  -> impl:Virtual_rules.t
  -> dir:Path.Build.t
  -> sctx:Super_context.t
  -> Module.t
  -> ml_kind:Ml_kind.t
  -> Module.t list Action_builder.t Memo.t
