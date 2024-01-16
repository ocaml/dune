open! Stdune
open! Import

val eval
  :  Dune_lang.Cond.t
  -> dir:Path.t
  -> f:Value.t list Memo.t String_with_vars.expander
  -> Value.t option Memo.t
