(** OCaml module compilation *)

open Import

(** Setup rules to build a single module.*)
val build_module :
  ?precompiled_cmi:bool -> Compilation_context.t -> Module.t -> unit Memo.t

val ocamlc_i :
     ?flags:string list
  -> deps:Module.t list Action_builder.t Ml_kind.Dict.t
  -> Compilation_context.t
  -> Module.t
  -> output:Path.Build.t
  -> unit Memo.t

val build_all : Compilation_context.t -> unit Memo.t

val with_empty_intf :
  sctx:Super_context.t -> dir:Path.Build.t -> Module.t -> Module.t Memo.t
