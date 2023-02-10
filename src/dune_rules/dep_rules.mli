(** Get dependencies for a set of modules using either ocamldep or ocamlobjinfo *)

open Import

val for_module :
     Ocamldep.Modules_data.t
  -> Module.t
  -> Module.t list Action_builder.t Ml_kind.Dict.t Memo.t

type graph =
  { compile : Dep_graph.t Ml_kind.Dict.t
  ; link : Dep_graph.t
  }

val dummy_graph : Module.t -> graph

val immediate_deps_of :
     Module.t
  -> Modules.t
  -> Path.Build.t Obj_dir.t
  -> Ml_kind.t
  -> Module.t list Action_builder.t

val rules : Ocamldep.Modules_data.t -> graph Memo.t
