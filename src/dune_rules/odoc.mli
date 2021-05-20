(** Odoc rules *)
open! Dune_engine

open! Stdune
open Import
open Dune_file

val setup_library_odoc_rules :
     Compilation_context.t
  -> Library.t
  -> dep_graphs:Dep_graph.Ml_kind.t
  -> unit Memo.Build.t

val global_rules : Super_context.t -> unit Memo.Build.t

val gen_rules :
  Super_context.t -> dir:Path.Build.t -> string list -> unit Memo.Build.t
