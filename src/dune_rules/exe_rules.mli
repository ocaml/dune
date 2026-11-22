val rules
  :  sctx:Super_context.t
  -> dir_contents:Dir_contents.t
  -> scope:Scope.t
  -> expander:Expander.t
  -> Executables.t
  -> (Compilation_context.t * Merlin.t) Memo.t
