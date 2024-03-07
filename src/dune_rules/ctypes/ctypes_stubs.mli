open Import

(* This module would be part of Ctypes_rules, except it creates a circular
   dependency if Dune_file tries to access it. *)

val libraries_needed_for_ctypes : loc:Loc.t -> Lib_dep.t list
