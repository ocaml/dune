(** The evaluation of a dune file *)

open Import

(** A fully evaluated dune file *)
type t

val dir : t -> Path.Local.t
val loaded_project : t -> Loaded_project.t
val output_dir : t -> Path.Build.t
val stanzas : t -> Stanza.t list Memo.t
val static_stanzas : t -> Stanza.t list
val project : t -> Dune_project.t
val to_dyn : t -> Dyn.t
val find_stanzas : t -> 'a Stanza.Key.t -> 'a list Memo.t
val fold_static_stanzas : t list -> init:'acc -> f:(t -> Stanza.t -> 'acc -> 'acc) -> 'acc

val eval
  :  (Loaded_project.t * Source_tree.Dir.t * Source.Dune_file.t) Appendable_list.t
  -> t list Per_context.t Memo.t

module Memo_fold : sig
  val fold_static_stanzas
    :  t list
    -> init:'acc
    -> f:(t -> Stanza.t -> 'acc -> 'acc Memo.t)
    -> 'acc Memo.t
end
