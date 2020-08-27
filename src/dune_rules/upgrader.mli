(** Upgrade projects from jbuilder to Dune *)
open! Dune_engine

(** Upgrade all projects in this file tree *)
val upgrade : unit -> unit
