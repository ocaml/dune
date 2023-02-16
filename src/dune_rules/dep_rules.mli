(** Get dependencies for a set of modules using either ocamldep or ocamlobjinfo *)

open Import

val current : project:Dune_project.t -> (module Dep_gen.S)

val for_module :
     Dep_gen.Modules_data.t
  -> Module.t
  -> Module.t list Action_builder.t Ml_kind.Dict.t Memo.t

val immediate_deps_of :
     project:Dune_project.t
  -> Module.t
  -> Modules.t
  -> Path.Build.t Obj_dir.t
  -> Ml_kind.t
  -> Module.t list Action_builder.t

val rules : Dep_gen.Modules_data.t -> Dep_graph.t Ml_kind.Dict.t Memo.t
