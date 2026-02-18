open Import

val foreign_rules
  :  Foreign_library.t
  -> sctx:Super_context.t
  -> expander:Expander.t
  -> dir:Path.Build.t
  -> dir_contents:Dir_contents.t
  -> unit Memo.t

val cm_files_for_lib : Library.t -> cctx:Compilation_context.t -> Cm_files.t

val compile_context
  :  Library.t
  -> sctx:Super_context.t
  -> dir_contents:Dir_contents.t
  -> expander:Expander.t
  -> scope:Scope.t
  -> Compilation_context.t Memo.t

val rules
  :  Library.t
  -> sctx:Super_context.t
  -> dir_contents:Dir_contents.t
  -> expander:Expander.t
  -> scope:Scope.t
  -> (Compilation_context.t * Merlin.t) Memo.t
