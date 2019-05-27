open! Stdune
open Dune_file

module Gen (S : sig val sctx : Super_context.t end) : sig
  val rules
    : Library.t
    -> dir_contents:Dir_contents.t
    -> dir:Path.Build.t
    -> expander:Expander.t
    -> scope:Scope.t
    -> dir_kind:Dune_lang.File_syntax.t
    -> Compilation_context.t * Merlin.t
end
