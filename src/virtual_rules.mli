open Stdune

val setup_copy_rules_for_impl
  :  sctx:Super_context.t
  -> dir:Path.t
  -> Vimpl.t
  -> unit

val impl
  :  Super_context.t
  -> dir:Path.t
  -> lib:Dune_file.Library.t
  -> scope:Scope.t
  -> modules:Module.Name_map.t
  -> Vimpl.t option
