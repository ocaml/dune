open Import

val gen_emit_rules :
     stanza_dir:Path.Build.t
  -> scope:Scope.t
  -> sctx:Super_context.t
  -> expander:Expander.t
  -> Melange_stanzas.Emit.t
  -> unit Memo.t
