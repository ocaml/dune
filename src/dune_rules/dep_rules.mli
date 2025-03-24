(** Get dependencies for a set of modules using either ocamldep or ocamlobjinfo *)

open Import

val for_module
  :  Ocamldep.Modules_data.t
  -> Module.t
  -> Module.t list Action_builder.t Ml_kind.Dict.t Memo.t

val immediate_deps_of
  :  Module.t
  -> Modules.With_vlib.t
  -> obj_dir:Path.Build.t Obj_dir.t
  -> ml_kind:Ml_kind.t
  -> Module.t list Action_builder.t

val rules : Ocamldep.Modules_data.t -> Dep_graph.t Ml_kind.Dict.t Memo.t
