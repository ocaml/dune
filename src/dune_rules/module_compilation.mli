(** OCaml module compilation *)

open Import

(** Setup rules to build a single module.*)
val build_module
  :  ?force_write_cmi:bool
  -> ?precompiled_cmi:bool
  -> Compilation_context.t
  -> Module.t
  -> unit Memo.t

(** Build a private signature re-exporting the aliases used during inference. *)
val build_inference_alias
  :  Compilation_context.t
  -> name:Module_name.t
  -> aliases:Module.t list
  -> Path.Build.t Memo.t

val ocamlc_i
  :  impl_deps:Module.t list Action_builder.t
  -> alias:Path.Build.t option
  -> Compilation_context.t
  -> Module.t
  -> output:Path.Build.t
  -> unit Memo.t

(** Infer the interface of a module and register it for promotion. *)
val infer_interface : Compilation_context.t -> Module.t -> unit Memo.t

val build_all : Compilation_context.t -> unit Memo.t

val with_empty_intf
  :  sctx:Super_context.t
  -> dir:Path.Build.t
  -> Module.t
  -> Module.t Memo.t

val melange_js_basename : Module.t -> Filename.t
