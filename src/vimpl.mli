(** Extra information required to generate rules for virtual library
    implementations *)

open Stdune

type t

val make
  :  vlib:Lib.t
  -> impl:Dune_file.Library.t
  -> dir:Path.t
  -> vlib_modules:Lib_modules.t
  -> vlib_foreign_objects:Path.t list
  -> vlib_dep_graph:Dep_graph.Ml_kind.t
  -> t

val impl : t -> Dune_file.Library.t

(** Return the library module information for the virtual library. Required for
    setting up the copying rules *)
val vlib_modules : t -> Lib_modules.t

val vlib : t -> Lib.t

val vlib_dep_graph : t -> Dep_graph.Ml_kind.t

(** Tests if the argument is a public module in the virtual library. This check
    is necessary so that we can assume the .cmi for this module is present when
    compiling implementations. *)
val is_public_vlib_module : t option -> Module.t -> bool

(** Combined implementation and virtual lib modules. Necessary to correctly
    interpret ocamldep results on implementations. *)
val add_vlib_modules : t option -> Module.Name_map.t -> Module.Name_map.t

(** All modules that have an implementation. These modules have object code that
    needs to be linked *)
val impl_only : t option -> Module.t list

(** Module list for creating an implementation's alias file *)
val aliased_modules : t option -> Lib_modules.t -> Module.Name_map.t

(** Find a module in the implementation if present or in its virtual library.
    This is required when introducing file dependencies on virtual library
    modules. *)
val find_module : t option -> Module.t -> Module.t option

(** Return the combined list of .o files for stubs consisting of .o files from
    the implementation and virtual library.*)
val vlib_stubs_o_files : t option -> Path.t list
