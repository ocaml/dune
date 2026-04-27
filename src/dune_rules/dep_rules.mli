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

(** Canonical [has_library_deps] for a library, used both by
    [Compilation_context.create] (when building a library cctx, to
    derive the [has_library_deps] passed to [rules]) and by
    [Compilation_context.build_lib_index] (when predicting whether
    the lib's [.d] file will exist for the cross-library walk).
    Returns [true] if [Lib.requires lib ~for_] resolves to a
    non-empty list, or if resolution fails (conservative — see the
    rationale comment in [dep_rules.ml]). *)
val has_library_deps_of_lib : Lib.t -> for_:Compilation_mode.t -> bool Memo.t

(** [has_library_deps] for non-library cctxes (executables, tests,
    melange emit), derived from already-resolved [direct] and
    [hidden] requires. Resolution failures conservatively fall
    back to has-deps=true — see the rationale comment in
    [dep_rules.ml]. *)
val has_library_deps_of_resolved
  :  direct:Lib.t list Resolve.Memo.t
  -> hidden:Lib.t list Resolve.Memo.t
  -> bool Memo.t

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
