open Stdune

type t

val lib
  :  lib:Dune_file.Library.t
  -> src_dir:Path.t
  -> user_written_modules:Module.Name_map.t
  -> main_module_name:Module.Name.t option
  -> mode:Wrapped.t
  -> t

val impl : t -> vlib:t -> t

val find_dep : t -> Module.Name.t -> Module.t option

val find : t -> Module.Name.t -> Module.t option

val compat_for_exn : t -> Module.t -> Module.t

val impl_only : t -> Module.t list

val singleton : Module.t -> t

val fold : t -> init:'acc -> f:(Module.t -> 'acc -> 'acc) -> 'acc
val fold_no_vlib : t -> init:'acc -> f:(Module.t -> 'acc -> 'acc) -> 'acc

val iter : t -> f:(Module.t -> unit) -> unit

val exe : Module.Name_map.t -> t

val alias_module : t -> Module.t option

val lib_interface : t -> Module.t option

val for_alias_exn : t -> Module.Name_map.t

(** This function doesn't really make sense for implementations, but it's only
    used for generating a comment in an alias file *)
val main_module_name_exn : t -> Module.Name.t

val fold_user_written
  :  t
  -> f:(Module.t -> 'acc -> 'acc)
  -> init:'acc
  -> 'acc

val map_user_written : t -> f:(Module.t -> Module.t) -> t

val wrapped_compat : t -> Module.Name_map.t

val for_odoc : t -> Module.t list

(** List of entry modules visible to users of the library. For wrapped
    libraries, this is always one module. For unwrapped libraries, this could be
    more than one. *)
val entry_modules : t -> Module.t list

val version_installed : t -> install_dir:Path.t -> t

val installable_modules : t -> Module.t list

val virtual_module_names : t -> Module.Name.Set.t

val decode : src_dir:Path.t -> t Dune_lang.Decoder.t

val encode : t -> Dune_lang.t list

val wrapped : t -> Wrapped.t

val real_unit_names : t -> Module.Name_map.t
