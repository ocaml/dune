open Import
open Dune_file

val foreign_rules :
     Foreign.Library.t
  -> sctx:Super_context.t
  -> expander:Expander.t
  -> dir:Path.Build.t
  -> dir_contents:Dir_contents.t
  -> unit Memo.t

val compile_info : Library.t -> Scope.t -> (Lib.t * Lib.Compile.t) Memo.t

val rules :
     Library.t
  -> sctx:Super_context.t
  -> dir_contents:Dir_contents.t
  -> dir:Path.Build.t
  -> expander:Expander.t
  -> scope:Scope.t
  -> (Compilation_context.t * Merlin.t) Memo.t
