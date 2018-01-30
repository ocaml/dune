(** OCaml module compilation *)

open Import

(** Setup rules to build a single module.

    [requires] must declare dependencies on files of libraries.
*)
val build_module
  :  Super_context.t
  -> ?sandbox:bool
  -> dynlink:bool
  -> js_of_ocaml:Jbuild.Js_of_ocaml.t
  -> flags:Ocaml_flags.t
  -> Module.t
  -> scope:Lib_db.Scope.t
  -> dir:Path.t
  -> dep_graph:Ocamldep.dep_graph
  -> modules:Module.t String_map.t
  -> requires:(unit, Lib.t list) Build.t Cm_kind.Dict.t
  -> alias_module:Module.t option
  -> unit

(** Setup rules to build all of [modules] *)
val build_modules
  :  Super_context.t
  -> dynlink:bool
  -> js_of_ocaml:Jbuild.Js_of_ocaml.t
  -> flags:Ocaml_flags.t
  -> scope:Lib_db.Scope.t
  -> dir:Path.t
  -> dep_graph:Ocamldep.dep_graph
  -> modules:Module.t String_map.t
  -> requires:(unit, Lib.t list) Build.t
  -> alias_module:Module.t option
  -> unit
