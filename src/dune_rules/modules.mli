(** Module layout information. Contains information about aliasing, wrapping. *)

open Import

type t

val to_dyn : t -> Dyn.t

val lib
  :  obj_dir:Path.Build.t
  -> main_module_name:Module_name.t option
  -> wrapped:Wrapped.t
  -> stdlib:Ocaml_stdlib.t option
  -> lib_name:Lib_name.Local.t
  -> implements:bool
  -> modules:Module.t Module_trie.t
  -> t

val decode : src_dir:Path.t -> t Dune_lang.Decoder.t
val fold : t -> init:'acc -> f:(Module.t -> 'acc -> 'acc) -> 'acc

module Group : sig
  type t

  val alias : t -> Module.t
  val lib_interface : t -> Module.t
  val for_alias : t -> (Module_name.t * Module.t) list
end

val exe_unwrapped : Module.t Module_trie.t -> obj_dir:Path.Build.t -> t

val make_wrapped
  :  obj_dir:Path.Build.t
  -> modules:Module.t Module_trie.t
  -> [ `Exe | `Melange ]
  -> t

val fold_user_written : t -> f:(Module.t -> 'acc -> 'acc) -> init:'acc -> 'acc
val map_user_written : t -> f:(Module.t -> Module.t Memo.t) -> t Memo.t
val fold_user_available : t -> f:(Module.t -> 'acc -> 'acc) -> init:'acc -> 'acc

module Sourced_module : sig
  type t =
    | Normal of Module.t
    | Imported_from_vlib of Module.t
    | Impl_of_virtual_module of Module.t Ml_kind.Dict.t

  val to_module : t -> Module.t
end

val obj_map : t -> Sourced_module.t Module_name.Unique.Map.t

(** Returns only the virtual module names in the library *)
val virtual_module_names : t -> Module_name.Path.Set.t

val wrapped : t -> Wrapped.t
val source_dirs : t -> Path.Set.t

module With_vlib : sig
  type modules := t
  type t

  val drop_vlib : t -> modules
  val to_dyn : t -> Dyn.t
  val encode : t -> src_dir:Path.t -> Dune_lang.t
  val impl : modules -> vlib:modules -> t

  val find_dep
    :  t
    -> of_:Module.t
    -> Module_name.t
    -> (Module.t list, [ `Parent_cycle ]) result

  val find : t -> Module_name.t -> Module.t option
  val compat_for_exn : t -> Module.t -> Module.t
  val impl_only : t -> Module.t list

  (** A set of modules from a single module. Not suitable for single module exe as
      this produce an unwrapped set of modules. Use [singleton_exe] instead for
      executables. *)
  val singleton : Module.t -> t

  val canonical_path : t -> Group.t -> Module.t -> Module_name.Path.t

  val fold_no_vlib_with_aliases
    :  t
    -> init:'acc
    -> normal:(Module.t -> 'acc -> 'acc)
    -> alias:(Group.t -> 'acc -> 'acc)
    -> 'acc

  (** For wrapped libraries, this is the user written entry module for the
      library. For single module libraries, it's the sole module in the library *)
  val lib_interface : t -> Module.t option

  (** Returns all the compatibility modules. *)
  val wrapped_compat : t -> Module.Name_map.t

  (** List of entry modules visible to users of the library. For wrapped
      libraries, this is always one module. For unwrapped libraries, this could be
      more than one. *)
  val entry_modules : t -> Module.t list

  (** Returns the main module name if it exists. It exist for libraries with
      [(wrapped true)] or one module libraries. *)
  val main_module_name : t -> Module_name.t option

  val version_installed : t -> src_root:Path.t -> install_dir:Path.t -> t
  val alias_for : t -> Module.t -> Module.t list
  val local_open : t -> Module.t -> Module_name.t list
  val is_stdlib_alias : t -> Module.t -> bool
  val exit_module : t -> Module.t option
  val as_singleton : t -> Module.t option

  type split_by_lib =
    { vlib : Module.t list
    ; impl : Module.t list
    }

  val split_by_lib : t -> split_by_lib

  (** [has_impl t] is true if there's at least one implementation in [t]*)
  val has_impl : t -> bool

  val modules : modules -> t
  val singleton_exe : Module.t -> t
  val obj_map : t -> Sourced_module.t Module_name.Unique.Map.t
  val wrapped : t -> Wrapped.t
end
