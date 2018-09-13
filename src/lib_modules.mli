open Stdune

type t = private
  { modules          : Module.Name_map.t
  ; virtual_modules  : Module.Name_map.t
  ; alias_module     : Module.t option
  ; main_module_name : Module.Name.t option
  ; wrapped_compat   : Module.Name_map.t
  }

val make
  :  Dune_file.Library.t
  -> dir:Path.t
  -> Module.Name_map.t
  -> virtual_modules:Module.Name_map.t
  -> main_module_name:Module.Name.t option
  -> t
