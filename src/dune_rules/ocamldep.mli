(** ocamldep management *)

open Import

val deps_of
  :  sandbox:Sandbox_config.t
  -> modules:Modules.With_vlib.t
  -> sctx:Super_context.t
  -> dir:Path.Build.t
  -> obj_dir:Path.Build.t Obj_dir.t
  -> ml_kind:Ml_kind.t
  -> Module.t
  -> Module.t list Action_builder.t Memo.t

val read_deps_of
  :  obj_dir:Path.Build.t Obj_dir.t
  -> modules:Modules.With_vlib.t
  -> ml_kind:Ml_kind.t
  -> Module.t
  -> Module.t list Action_builder.t

(** [read_immediate_deps_of ~obj_dir ~modules ~ml_kind unit] returns the
    immediate dependencies found in the modules of [modules] for the file with
    kind [ml_kind] of the module [unit]. If there is no such file with kind
    [ml_kind], then an empty list of dependencies is returned. *)
val read_immediate_deps_of
  :  obj_dir:Path.Build.t Obj_dir.t
  -> modules:Modules.With_vlib.t
  -> ml_kind:Ml_kind.t
  -> Module.t
  -> Module.t list Action_builder.t
