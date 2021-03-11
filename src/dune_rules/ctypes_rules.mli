open! Dune_engine
open Import

module Ctypes = Dune_file.Ctypes

val libraries_needed_for_ctypes : loc:Loc.t -> Lib_dep.t list

val generated_ml_and_c_files : Ctypes.t -> string list

val gen_rules :
     dep_graphs : Dep_graph.t Ml_kind.Dict.t
  -> cctx : Compilation_context.t
  -> buildable : Dune_file.Buildable.t
  -> loc : Loc.t
  -> scope : Scope.t
  -> dir : Path.Build.t
  -> sctx : Super_context.t
  -> unit
