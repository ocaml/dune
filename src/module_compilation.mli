(** OCaml module compilation *)

open Import

(** Setup rules to build a single module.*)
val build_module
  :  dep_graphs:Dep_graph.Ml_kind.t
  -> Compilation_context.t
  -> Module.t
  -> unit

val ocamlc_i
  :  ?flags:string list
  -> dep_graphs:Dep_graph.Ml_kind.t
  -> Compilation_context.t
  -> Module.t
  -> output:Path.Build.t
  -> unit
