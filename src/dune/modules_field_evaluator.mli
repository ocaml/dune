(** The modules field evaluator inteprets the module sources into proper modules
    in a directroy in the context of a stanza from a dune file. *)
open! Stdune

module Virtual : sig
  type t = { virtual_modules : Ordered_set_lang.t }
end

module Implementation : sig
  type t =
    { existing_virtual_modules : Module_name.Set.t
    ; allow_new_public_modules : bool
    }
end

type kind =
  | Virtual of Virtual.t
  | Implementation of Implementation.t
  | Exe_or_normal_lib

val eval :
     modules:Module.Source.t Module_name.Map.t
  -> buildable:Dune_file.Buildable.t
  -> private_modules:Ordered_set_lang.t
  -> kind:kind
  -> Module.Name_map.t
