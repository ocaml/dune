(** Extra information required to generate rules for virtual library
    implementations *)

open Import

type t

val make
  :  sctx:Super_context.t
  -> scope:Scope.t
  -> lib:Library.t
  -> info:Path.t Lib_info.t
  -> vlib:Lib.t
  -> for_:Compilation_mode.t
  -> t Memo.t

val impl : t -> Library.t

(** Return the library module information for the virtual library. Required for
    setting up the copying rules *)
val vlib_modules : t -> Modules.t

val vlib : t -> Lib.t

(** Return the combined list of .o files for stubs consisting of .o files from
    the implementation and virtual library.*)
val vlib_stubs_o_files : t -> Path.t list

val impl_cm_kind : t -> Cm_kind.t
val vlib_obj_map : t -> Modules.Sourced_module.t Module_name.Unique.Map.t
val setup_copy_rules : sctx:Super_context.t -> dir:Path.Build.t -> t -> unit Memo.t
