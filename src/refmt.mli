(** refmt is the binary that is used to convert reason syntax to OCaml and
    reformat reason source *)
open Stdune

type t

val get : Super_context.t -> loc:Loc.t option -> dir:Path.Build.t -> t

val format : t -> input:Path.t -> output:Path.Build.t -> Action.t Build.s

val to_ocaml_ast : t -> input:Path.t -> output:Path.Build.t -> Action.t Build.s
