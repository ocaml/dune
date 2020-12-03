(** Upgrade projects from jbuilder to Dune *)
open! Build_api.Api

(** Upgrade all projects in this file tree *)
val upgrade : unit -> unit
