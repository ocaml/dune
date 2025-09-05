open! Stdune
open! Import

(** Evaluate a cond statement, choosing the first value whose condition was met
    or [None] if none of the conditions were met. *)
val eval
  :  Dune_lang.Cond.t
  -> dir:Path.t
  -> f:Value.t list Memo.t String_with_vars.expander
  -> Value.t option Memo.t
