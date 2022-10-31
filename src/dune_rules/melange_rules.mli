open Import

val compiler : sctx:Super_context.t -> dir:Path.Build.t -> Action.Prog.t Memo.t

val gen_rules :
     melange_stanza_dir:Path.Build.t
  -> scope:Scope.t
  -> sctx:Super_context.t
  -> expander:Expander.t
  -> Melange_stanzas.Emit.t
  -> unit Memo.t
