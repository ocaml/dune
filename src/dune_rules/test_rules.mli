open! Dune_engine
open Stdune

val rules :
     Dune_file.Tests.t
  -> sctx:Super_context.t
  -> dir:Path.Build.t
  -> scope:Scope.t
  -> expander:Expander.t
  -> dir_contents:Dir_contents.t
  -> Compilation_context.t * Merlin.t
