(** The evaluation of a dune file *)

open Import

(** A fully evaluated dune file *)
type t

val dir : t -> Source_path.t
val loaded_dir : t -> Loaded_dir.t
val source_dir : t -> Source_path.t
val output_dir : t -> Path.Build.t
val stanzas : t -> Stanza.t list Memo.t
val static_stanzas : t -> Stanza.t list
val project : t -> Dune_project.t
val to_dyn : t -> Dyn.t
val find_stanzas : t -> 'a Stanza.Key.t -> 'a list Memo.t
val fold_static_stanzas : t list -> init:'acc -> f:(t -> Stanza.t -> 'acc -> 'acc) -> 'acc

val eval
  :  (Loaded_project.t * Source_tree.Rules.Dir.t * Source.Dune_file.t) Appendable_list.t
  -> Only_packages.t
  -> t list Per_context.t Memo.t

module Memo_fold : sig
  val fold_static_stanzas
    :  t list
    -> init:'acc
    -> f:(t -> Stanza.t -> 'acc -> 'acc Memo.t)
    -> 'acc Memo.t
end
