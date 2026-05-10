(** Get dependencies for a set of modules using either ocamldep or ocamlobjinfo *)

open Import

val for_module
  :  obj_dir:Path.Build.t Obj_dir.t
  -> modules:Modules.With_vlib.t
  -> sandbox:Sandbox_config.t
  -> impl:Virtual_rules.t
  -> dir:Path.Build.t
  -> sctx:Super_context.t
  -> for_:Compilation_mode.t
  -> Module.t
  -> Module.t list Action_builder.t Ml_kind.Dict.t Memo.t

(** Single-module stanzas short-circuit ocamldep when [not
    has_library_deps] or [for_ = Melange]: the per-module filter
    that consumes the dep graph doesn't activate in those cases. *)
val rules
  :  obj_dir:Path.Build.t Obj_dir.t
  -> modules:Modules.With_vlib.t
  -> sandbox:Sandbox_config.t
  -> impl:Virtual_rules.t
  -> sctx:Super_context.t
  -> dir:Path.Build.t
  -> for_:Compilation_mode.t
  -> has_library_deps:bool
  -> Dep_graph.Ml_kind.t Memo.t

val read_immediate_deps_of
  :  sandbox:Sandbox_config.t
  -> sctx:Super_context.t
  -> obj_dir:Path.Build.t Obj_dir.t
  -> modules:Modules.With_vlib.t
  -> ml_kind:Ml_kind.t
  -> Module.t
  -> Module.t list Action_builder.t

val read_deps_of
  :  sandbox:Sandbox_config.t
  -> sctx:Super_context.t
  -> obj_dir:Path.Build.t Obj_dir.t
  -> modules:Modules.With_vlib.t
  -> impl:Virtual_rules.t
  -> dir:Path.Build.t
  -> for_:Compilation_mode.t
  -> ml_kind:Ml_kind.t
  -> Module.t
  -> Module.t list Action_builder.t
