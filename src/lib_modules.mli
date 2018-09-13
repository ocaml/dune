open Stdune

type t

module Alias_module : sig
  type t =
    { module_name : Module.Name.t
    ; alias_module : Module.t
    }
end

val alias : t -> Alias_module.t option

val modules : t -> Module.Name_map.t

val wrapped_compat : t -> Module.Name_map.t

val lib_interface_module : t -> Module.t option

val virtual_modules : t -> Module.Name_map.t

val installable_modules : t -> Module.t list

val entry_modules : t -> Module.t list

val make
  :  Dune_file.Library.t
  -> dir:Path.t
  -> Module.Name_map.t
  -> virtual_modules:Module.Name_map.t
  -> main_module_name:Module.Name.t option
  -> t
