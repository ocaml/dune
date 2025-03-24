open Import

val compile_info : scope:Scope.t -> Executables.t -> Lib.Compile.t Memo.t

val rules
  :  sctx:Super_context.t
  -> dir:Path.Build.t
  -> dir_contents:Dir_contents.t
  -> scope:Scope.t
  -> expander:Expander.t
  -> Executables.t
  -> (Compilation_context.t * Merlin.t) Memo.t
