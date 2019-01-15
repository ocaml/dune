open Stdune

type t

val alias_module : t -> Module.t option

val modules : t -> Module.Name_map.t

val wrapped_compat : t -> Module.Name_map.t

val main_module_name : t -> Module.Name.t option

val virtual_modules : t -> Module.Name_map.t

val installable_modules : t -> Module.t list

val lib_interface_module : t -> Module.t option

val entry_modules : t -> Module.t list

val has_private_modules : t -> bool

val public_modules : t -> Module.Name_map.t

val make
  :  Dune_file.Library.t
  -> obj_dir:Obj_dir.t
  -> Module.Name_map.t
  -> main_module_name:Module.Name.t option
  -> wrapped:Wrapped.t
  -> t

val set_modules : t -> Module.Name_map.t -> t

val version_installed : t -> install_dir:Path.t -> t

val for_compilation : t -> Module.Name_map.t

val have_artifacts : t -> Module.Name_map.t

val for_alias : t -> Module.Name_map.t

val encode : t -> Dune_lang.t list

val decode : implements:bool -> dir:Path.t -> t Dune_lang.Decoder.t

val is_wrapped : t -> bool

val wrapped : t -> Wrapped.t

val needs_alias_module : t -> bool
