(** Get dependencies for a set of modules using either ocamldep or ocamlobjinfo *)

val rules :
  Compilation_context.t -> modules:Modules.t -> Dep_graph.t Ml_kind.Dict.t
