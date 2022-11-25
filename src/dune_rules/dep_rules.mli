(** Get dependencies for a set of modules using either ocamldep or ocamlobjinfo *)

open Import

module Current_dep_gen : Dep_gen.S

val for_module :
     Dep_gen.Modules_data.t
  -> Module.t
  -> Module.t list Action_builder.t Ml_kind.Dict.t Memo.t

val rules : Dep_gen.Modules_data.t -> Dep_graph.t Ml_kind.Dict.t Memo.t
