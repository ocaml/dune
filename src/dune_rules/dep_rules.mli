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

(** [has_library_deps] indicates whether the enclosing stanza declares any
    library dependencies. When false, single-module stanzas short-circuit
    ocamldep entirely (no build rule, no [.d]/[.all-deps] file). When
    true, single-module stanzas run ocamldep so that the per-module
    inter-library dependency filter can determine which libraries the
    single module references. *)
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
  :  obj_dir:Path.Build.t Obj_dir.t
  -> modules:Modules.With_vlib.t
  -> ml_kind:Ml_kind.t
  -> for_:Compilation_mode.t
  -> Module.t
  -> Module.t list Action_builder.t

val read_deps_of
  :  obj_dir:Path.Build.t Obj_dir.t
  -> modules:Modules.With_vlib.t
  -> ml_kind:Ml_kind.t
  -> for_:Compilation_mode.t
  -> Module.t
  -> Module.t list Action_builder.t
