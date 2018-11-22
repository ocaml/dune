val rules :
  sctx:Super_context.t ->
  rctx:Rule_context.t ->
  dir:Stdune.Path.t ->
  dir_contents:Dir_contents.t ->
  scope:Scope.t ->
  expander:Expander.t ->
  dir_kind:Dune_lang.syntax ->
  Dune_file.Executables.t ->
  Compilation_context.t * Merlin.t
