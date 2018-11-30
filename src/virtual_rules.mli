open Stdune

module Gen (S : sig val sctx : Super_context.t end) : sig
  val setup_copy_rules_for_impl
    :  dir:Path.t
    -> Vimpl.t
    -> unit

  val impl
    :  lib:Dune_file.Library.t
    -> scope:Scope.t
    -> modules:Module.Name_map.t
    -> Vimpl.t option
end
