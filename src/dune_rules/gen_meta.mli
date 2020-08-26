(** Generate a META file *)
open! Dune_engine

open! Import

val gen :
     package:Package.t
  -> add_directory_entry:bool
  -> Super_context.Lib_entry.t list
  -> Meta.t
