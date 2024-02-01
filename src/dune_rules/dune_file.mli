(** The evaluation of a dune file *)

open Import

(** A fully evaluated dune file *)
type t

val dir : t -> Path.Source.t
val stanzas : t -> Stanza.t list
val set_stanzas : t -> Stanza.t list -> t
val project : t -> Dune_project.t
val equal : t -> t -> bool
val hash : t -> int
val to_dyn : t -> Dyn.t
val find_stanzas : t -> 'a Stanza.Key.t -> 'a list
val fold_stanzas : t list -> init:'acc -> f:(t -> Stanza.t -> 'acc -> 'acc) -> 'acc

val eval
  :  (Path.Source.t * Dune_project.t * Dune_file0.t) Appendable_list.t
  -> (Context_name.t -> t list Memo.t) Staged.t Memo.t

module Memo_fold : sig
  val fold_stanzas
    :  t list
    -> init:'acc
    -> f:(t -> Stanza.t -> 'acc -> 'acc Memo.t)
    -> 'acc Memo.t
end
