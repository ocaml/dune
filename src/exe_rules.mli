open Stdune

val rules
  :  sctx:Super_context.t
  -> dir:Path.Build.t
  -> dir_contents:Dir_contents.t
  -> scope:Scope.t
  -> expander:Expander.t
  -> dir_kind:Dune_lang.File_syntax.t
  -> Dune_file.Executables.t
  -> Compilation_context.t * Merlin.t
