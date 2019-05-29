open Stdune

val rules
  :  Dune_file.Tests.t
  -> sctx:Super_context.t
  -> dir:Path.Build.t
  -> scope:Scope.t
  -> expander:Expander.t
  -> dir_contents:Dir_contents.t
  -> dir_kind:Dune_lang.File_syntax.t
  -> Compilation_context.t * Merlin.t
