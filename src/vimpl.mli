(** Extra information required to generate rules for virtual library
    implementations *)

open Stdune

type t

val make
  :  vlib:Lib.t
  -> impl:Dune_file.Library.t
  -> vlib_modules:Lib_modules.t
  -> vlib_dep_graph:Dep_graph.Ml_kind.t
  -> t

val impl : t -> Dune_file.Library.t

val vlib_modules : t -> Lib_modules.t

val vlib : t -> Lib.t

val vlib_dep_graph : t -> Dep_graph.Ml_kind.t

val is_public_vlib_module : t option -> Module.t -> bool

val add_vlib_modules : t option -> Module.Name_map.t -> Module.Name_map.t

val impl_only : t option -> Module.t list

val aliased_modules : t option -> Lib_modules.t -> Module.Name_map.t

val find_module : t option -> Module.t -> Module.t option

val vlib_stubs_o_files : t option -> Path.t list

val for_file_deps : t option -> Module.t list -> Module.t list
