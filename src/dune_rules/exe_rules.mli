open Import

val compile_info : scope:Scope.t -> Dune_file.Executables.t -> Lib.Compile.t Memo.t

val rules
  :  sctx:Super_context.t
  -> dir:Path.Build.t
  -> dir_contents:Dir_contents.t
  -> scope:Scope.t
  -> expander:Expander.t
  -> Dune_file.Executables.t
  -> (Compilation_context.t * Merlin.t) Memo.t
