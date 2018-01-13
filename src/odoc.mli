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

val gen_rules : Super_context.t -> dir:Path.t -> string list -> unit
