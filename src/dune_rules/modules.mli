(** Module layout information. Contains information about aliasing, wrapping. *)

open Import

type t

val to_dyn : t -> Dyn.t

val equal : t -> t -> bool

val lib :
     src_dir:Path.Build.t
  -> main_module_name:Module_name.t option
  -> wrapped:Wrapped.t
  -> stdlib:Ocaml_stdlib.t option
  -> lib_name:Lib_name.Local.t
  -> implements:bool
  -> modules:Module.Name_map.t
  -> t

val encode : t -> Dune_lang.t

val decode : src_dir:Path.t -> t Dune_lang.Decoder.t

val impl : t -> vlib:t -> t

val find_dep : t -> of_:Module.t -> Module_name.t -> Module.t option

val find : t -> Module_name.t -> Module.t option

val compat_for_exn : t -> Module.t -> Module.t

val impl_only : t -> Module.t list

(** A set of modules from a single module. Not suitable for single module exe as
    this produce an unwrapped set of modules. Use [singleton_exe] instead for
    executables. *)
val singleton : Module.t -> t

val singleton_exe : Module.t -> t

val fold_no_vlib : t -> init:'acc -> f:(Module.t -> 'acc -> 'acc) -> 'acc

val exe_unwrapped : Module.Name_map.t -> t

val make_wrapped :
  src_dir:Path.Build.t -> modules:Module.Name_map.t -> [ `Exe | `Melange ] -> t

(** For wrapped libraries, this is the user written entry module for the
    library. For single module libraries, it's the sole module in the library *)
val lib_interface : t -> Module.t option

(** Returns the modules that need to be aliased in the alias module *)
val for_alias : t -> Module.Name_map.t

val fold_user_written : t -> f:(Module.t -> 'acc -> 'acc) -> init:'acc -> 'acc

val map_user_written : t -> f:(Module.t -> Module.t Memo.t) -> t Memo.t

val map : t -> f:(Module.t -> Module.t) -> t

val fold_user_available : t -> f:(Module.t -> 'acc -> 'acc) -> init:'acc -> 'acc

(** Returns all the compatibility modules. *)
val wrapped_compat : t -> Module.Name_map.t

module Sourced_module : sig
  type t =
    | Normal of Module.t
    | Imported_from_vlib of Module.t
    | Impl_of_virtual_module of Module.t Ml_kind.Dict.t
end

val obj_map : t -> f:(Sourced_module.t -> 'a) -> 'a Module.Obj_map.t

val obj_map_build :
  t -> f:(Sourced_module.t -> 'a Memo.t) -> 'a Module.Obj_map.t Memo.t

(** List of entry modules visible to users of the library. For wrapped
    libraries, this is always one module. For unwrapped libraries, this could be
    more than one. *)
val entry_modules : t -> Module.t list

(** Returns the main module name if it exists. It exist for libraries with
    [(wrapped true)] or one module libraries. *)
val main_module_name : t -> Module_name.t option

(** Returns only the virtual module names in the library *)
val virtual_module_names : t -> Module_name.Set.t

(** Returns the alias module if it exists. This module only exists for
    [(wrapped true)] and when there is more than 1 module. *)
val alias_module : t -> Module.t option

val wrapped : t -> Wrapped.t

val version_installed : t -> install_dir:Path.t -> t

val alias_for : t -> Module.t -> Module.t option

val is_stdlib_alias : t -> Module.t -> bool

val exit_module : t -> Module.t option

(** [relocate_alias_module t ~src_dir] sets the source directory of the alias
    module to [src_dir]. Only works if [t] is wrapped. *)
val relocate_alias_module : t -> src_dir:Path.t -> t

val as_singleton : t -> Module.t option

val source_dirs : t -> Path.Set.t

type split_by_lib =
  { vlib : Module.t list
  ; impl : Module.t list
  }

val split_by_lib : t -> split_by_lib

(** [has_impl t] is true if there's at least one implementation in [t]*)
val has_impl : t -> bool
