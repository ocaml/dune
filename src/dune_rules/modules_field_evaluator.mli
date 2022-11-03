(** The modules field evaluator interprets the module sources into proper
    modules in a directory in the context of a stanza from a dune file. *)

open Import

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
  -> stanza_loc:Loc.t
  -> modules_field:Ordered_set_lang.t
  -> modules_without_implementation:Ordered_set_lang.t
  -> root_module:('a * Module_name.t) option
  -> private_modules:Ordered_set_lang.t
  -> kind:kind
  -> src_dir:Path.Build.t
  -> Module.t Module_name.Map.t
