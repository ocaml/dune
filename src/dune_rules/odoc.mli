(** Odoc rules *)
open! Build_api.Api

open! Stdune
open Dune_file

val setup_library_odoc_rules :
  Compilation_context.t -> Library.t -> dep_graphs:Dep_graph.Ml_kind.t -> unit

val init : Super_context.t -> unit

val gen_rules : Super_context.t -> dir:Path.Build.t -> string list -> unit
