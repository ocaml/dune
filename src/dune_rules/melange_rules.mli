open Import

val compile_info :
  scope:Scope.t -> Melange_stanzas.Emit.t -> Lib.Compile.t Memo.t

val emit_rules :
     dir_contents:Dir_contents.t
  -> dir:Path.Build.t
  -> scope:Scope.t
  -> sctx:Super_context.t
  -> expander:Expander.t
  -> Melange_stanzas.Emit.t
  -> (Compilation_context.t * Merlin.t) Memo.t
