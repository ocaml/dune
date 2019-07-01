(** ocamldep management *)

open Stdune

(** Generate ocamldep rules for all the modules in the context. *)
val rules
  :  Compilation_context.t
  -> modules:Module.Name_map.t
  -> Dep_graph.Ml_kind.t

(** Get the dep graph for an already defined library *)
val graph_of_remote_lib
  :  obj_dir:Path.Build.t Obj_dir.t
  -> modules:Module.t Module.Name.Map.t
  -> Dep_graph.Ml_kind.t
