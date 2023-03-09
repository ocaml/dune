open Import

val emit_target_dir : Melange_stanzas.Emit.t -> dir:Path.Build.t -> Path.Build.t

val setup_emit_cmj_rules :
     sctx:Super_context.t
  -> dir:Path.Build.t
  -> scope:Scope.t
  -> expander:Expander.t
  -> dir_contents:Dir_contents.t
  -> Melange_stanzas.Emit.t
  -> (Compilation_context.t * Merlin.t) Memo.t

val setup_emit_js_rules :
     dir_contents:Dir_contents.t
  -> dir:Path.Build.t
  -> scope:Scope.t
  -> sctx:Super_context.t
  -> Melange_stanzas.Emit.t
  -> unit Memo.t

val eval_runtime_deps :
  expander:Expander.t -> Dep_conf.t list -> Path.Set.t Memo.t

val raise_external_runtime_dep_error : loc:Loc.t -> Lib_name.t -> Path.t -> 'a
