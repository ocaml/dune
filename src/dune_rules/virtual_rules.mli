open! Dune_engine
open Stdune

val setup_copy_rules_for_impl :
  sctx:Super_context.t -> dir:Path.Build.t -> Vimpl.t -> unit Memo.Build.t

val impl :
     Super_context.t
  -> lib:Dune_file.Library.t
  -> scope:Scope.t
  -> Vimpl.t option Memo.Build.t
