(** Odoc rules *)

open Import
open Jbuild

val setup_library_rules
  :  Super_context.t
  -> Library.t
  -> dir:Path.t
  -> modules:Module.t String_map.t
  -> mld_files:string list
  -> requires:(unit, Lib.t list) Build.t
  -> dep_graph:Ocamldep.dep_graph
  -> unit

val setup_css_rule : Super_context.t -> unit

val setup_toplevel_index_rule: Super_context.t -> unit
