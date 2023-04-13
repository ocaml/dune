open Import

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
