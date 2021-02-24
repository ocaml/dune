(* Separate from [Dune_load] to break dependency cycles *)

open! Dune_engine
open! Stdune

(** A fully evaluated dune file *)
type t =
  { dir : Path.Source.t
  ; project : Dune_project.t
  ; stanzas : Dune_file.Stanzas.t
  }

val parse :
     Dune_lang.Ast.t list
  -> dir:Path.Source.t
  -> file:Path.Source.t
  -> project:Dune_project.t
  -> t

val fold_stanzas :
  t list -> init:'acc -> f:(t -> Stanza.t -> 'acc -> 'acc) -> 'acc
