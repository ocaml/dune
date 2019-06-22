(** Odoc rules *)

open! Stdune
open Import
open Dune_file

val setup_library_odoc_rules
  :  Super_context.t
  -> Library.t
  -> obj_dir:Path.Build.t Obj_dir.t
  -> scope:Scope.t
  -> modules:Module.t list
  -> requires:Lib.t list Or_exn.t
  -> dep_graphs:Dep_graph.Ml_kind.t
  -> unit

val init : Super_context.t -> unit

val gen_rules : Super_context.t -> dir:Path.Build.t -> string list -> unit
