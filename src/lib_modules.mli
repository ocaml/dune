(** Module information for libraries. Contains extra information about aliased
    modules, lib interface modules, wrapping options. *)

open! Stdune

type t

(** Returns the alias module if it exists. This module only exists for [(wrapped
    true)] and when there is more than 1 module. *)
val alias_module : t -> Module.t option

val modules : t -> Module.Name_map.t

(** Returns all the compatibility modules. These modules are compiled
    separately *)
val wrapped_compat : t -> Module.Name_map.t

(** Returns the main module name if it exists. It exist for libraries with
   [(wrapped true)] or one module libraries. *)
val main_module_name : t -> Module.Name.t option

(** Returns only the virtual modules in the library *)
val virtual_modules : t -> Module.Name_map.t

(** All modules that need to be considered for installation. *)
val installable_modules : t -> Module.t list

(** For wrapped libraries, this is the user written entry module for the
    library. For single module libraries, it's the sole module in the library *)
val lib_interface_module : t -> Module.t option

val make
  :  Dune_file.Library.t
  -> src_dir:Path.t
  -> Module.Name_map.t
  -> main_module_name:Module.Name.t option
  -> wrapped:Wrapped.t
  -> t

val set_modules : t -> Module.Name_map.t -> t

val encode : t -> Dune_lang.t list

val decode
  : implements:bool -> src_dir:Path.t -> t Dune_lang.Decoder.t

val wrapped : t -> Wrapped.t

val version_installed : t -> install_dir:Path.t -> t
