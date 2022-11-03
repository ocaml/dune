open Import

val gen_emit_rules :
     dir_contents:Dir_contents.t
  -> dir:Path.Build.t
  -> scope:Scope.t
  -> sctx:Super_context.t
  -> expander:Expander.t
  -> Melange_stanzas.Emit.t
  -> unit Memo.t
