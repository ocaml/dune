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

(** List of entry modules visible to users of the library. For wrapped
    libraries, this is always one module. For unwrapped libraries, this could be
    more than one. *)
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

(** [version_installed t ~install_dir] converts [t] to a version that represents
    a library installed [install_dir]*)
val version_installed : t -> install_dir:Obj_dir.t -> t

(** Return all modules that need to be compiled. Includes the alias module if it
    exists. This does not include the compatibility modules which are compiled
    separately *)
val for_compilation : t -> Module.Name_map.t

(** Returns the modules that need to be aliased in the alias module *)
val for_alias : t -> Module.Name_map.t

val encode : t -> Dune_lang.t list

val decode : implements:bool -> obj_dir:Obj_dir.t -> t Dune_lang.Decoder.t

val is_wrapped : t -> bool

val wrapped : t -> Wrapped.t

(** Returns true if the collection of modules represented here needs an alias
    module. I.e. it's wrapped and has more than one module. *)
val needs_alias_module : t -> bool
