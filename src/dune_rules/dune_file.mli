(** The evaluation of a dune file *)

open Import

(** A fully evaluated dune file *)
type t

val dir : t -> Path.Source.t
val stanzas : t -> Stanza.t list Memo.t
val static_stanzas : t -> Stanza.t list
val project : t -> Dune_project.t
val to_dyn : t -> Dyn.t
val find_stanzas : t -> 'a Stanza.Key.t -> 'a list Memo.t
val fold_static_stanzas : t list -> init:'acc -> f:(t -> Stanza.t -> 'acc -> 'acc) -> 'acc

(** The stanzas of a dune file that do not depend on the build context,
    including those dropped by [--only-packages], a false [enabled_if] field
    of a package, or [--ignore-promoted-rules]. They are [Unknown] for dune
    files in OCaml syntax, with dynamic includes, or with rule plugin
    invocations. *)
module Unmasked : sig
  type t =
    | Known of Stanza.t list
    | Unknown of Dune_project.t
end

(** Also returns the unmasked stanzas of every dune file. *)
val eval
  :  (Path.Source.t * Dune_project.t * Source.Dune_file.t) Appendable_list.t
  -> Only_packages.t
  -> (Unmasked.t list * t list Per_context.t) Memo.t

module Memo_fold : sig
  val fold_static_stanzas
    :  t list
    -> init:'acc
    -> f:(t -> Stanza.t -> 'acc -> 'acc Memo.t)
    -> 'acc Memo.t
end
