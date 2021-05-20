(** OCaml module compilation *)
open! Dune_engine

open Import

(** Setup rules to build a single module.*)
val build_module :
     dep_graphs:Dep_graph.Ml_kind.t
  -> ?precompiled_cmi:bool
  -> Compilation_context.t
  -> Module.t
  -> unit Memo.Build.t

val ocamlc_i :
     ?flags:string list
  -> deps:Module.t list Action_builder.t Ml_kind.Dict.t
  -> Compilation_context.t
  -> Module.t
  -> output:Path.Build.t
  -> unit Memo.Build.t

val build_all :
  Compilation_context.t -> dep_graphs:Dep_graph.Ml_kind.t -> unit Memo.Build.t
