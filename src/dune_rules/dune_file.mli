(** Representation and parsing of Dune files *)

open Import

val stanza_package : Stanza.t -> Package.t option

(** [of_ast project ast] is the list of [Stanza.t]s derived from decoding the
    [ast] according to the syntax given by [kind] in the context of the
    [project] *)
val of_ast : Dune_project.t -> Dune_lang.Ast.t -> Stanza.t list

(** A fully evaluated dune file *)
type t =
  { dir : Path.Source.t
  ; project : Dune_project.t
  ; stanzas : Stanza.t list
  }

val equal : t -> t -> bool
val hash : t -> int
val to_dyn : t -> Dyn.t

val parse
  :  Dune_lang.Ast.t list
  -> dir:Path.Source.t
  -> file:Path.Source.t option
  -> project:Dune_project.t
  -> t Memo.t

val fold_stanzas : t list -> init:'acc -> f:(t -> Stanza.t -> 'acc -> 'acc) -> 'acc

module Memo_fold : sig
  val fold_stanzas
    :  t list
    -> init:'acc
    -> f:(t -> Stanza.t -> 'acc -> 'acc Memo.t)
    -> 'acc Memo.t
end
