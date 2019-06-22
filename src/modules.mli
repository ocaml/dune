type t

val lib : Lib_modules.t -> t

val impl : t -> vlib:t -> t

val find_dep : t -> Module.Name.t -> Module.t option

val find : t -> Module.Name.t -> Module.t option

val compat_for_exn : t -> Module.t -> Module.t

val impl_only : t -> Module.t list

val singleton : Module.t -> t

val fold : t -> init:'acc -> f:(Module.t -> 'acc -> 'acc) -> 'acc

val iter : t -> f:(Module.t -> unit) -> unit

val exe : Module.Name_map.t -> t

val lib_interface : t -> Module.t option

val for_alias_exn : t -> Module.Name_map.t

val main_module_name_exn : t -> Module.Name.t

val fold_user_written
  :  t
  -> f:(Module.t -> 'acc -> 'acc)
  -> init:'acc
  -> 'acc

val map_user_written : t -> f:(Module.t -> Module.t) -> t

val wrapped_compat : t -> Module.Name_map.t
