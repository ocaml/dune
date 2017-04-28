(** OCaml module compilation *)

open Import

(** Setup rules to build a single module *)
val build_module
  :  Super_context.t
  -> ?sandbox:bool
  -> dynlink:bool
  -> flags:Ocaml_flags.t
  -> Module.t
  -> dir:Path.t
  -> dep_graph:Ocamldep.dep_graph
  -> modules:Module.t String_map.t
  -> requires:(unit, Lib.t list) Build.t
  -> alias_module:Module.t option
  -> unit

(** Setup rules to build all of [modules] *)
val build_modules
  :  Super_context.t
  -> dynlink:bool
  -> flags:Ocaml_flags.t
  -> dir:Path.t
  -> dep_graph:Ocamldep.dep_graph
  -> modules:Module.t String_map.t
  -> requires:(unit, Lib.t list) Build.t
  -> alias_module:Module.t option
  -> unit
