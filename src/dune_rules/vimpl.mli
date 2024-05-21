(** Extra information required to generate rules for virtual library
    implementations *)

open Import

type t

val make
  :  vlib:Lib.t
  -> impl:Library.t
  -> vlib_modules:Modules.t
  -> vlib_foreign_objects:Path.t list
  -> t

val impl : t -> Library.t

(** Return the library module information for the virtual library. Required for
    setting up the copying rules *)
val vlib_modules : t -> Modules.t

val impl_modules : t option -> Modules.t -> Modules.With_vlib.t
val vlib : t -> Lib.t

(** Return the combined list of .o files for stubs consisting of .o files from
    the implementation and virtual library.*)
val vlib_stubs_o_files : t option -> Path.t list

val impl_cm_kind : t -> Cm_kind.t
val vlib_obj_map : t -> Modules.Sourced_module.t Module_name.Unique.Map.t
