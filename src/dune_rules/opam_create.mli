(** Generate opam files from dune-project metadata *)
open! Dune_engine

open Stdune

(** Given an opam filename, returns the filename of the template file *)
val template_file : Path.t -> Path.t

(** Generate the contents of an opam file. [template] is the filename and
    contents of the template file. *)
val generate :
  Dune_project.t -> Package.t -> template:(Path.t * string) option -> string

val add_rules : Super_context.t -> dir:Path.Build.t -> unit
