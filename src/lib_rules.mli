open! Stdune
open Dune_file

val rules
  :  Library.t
  -> sctx:Super_context.t
  -> dir_contents:Dir_contents.t
  -> dir:Path.Build.t
  -> expander:Expander.t
  -> scope:Scope.t
  -> dir_kind:Dune_lang.File_syntax.t
  -> Compilation_context.t * Merlin.t
