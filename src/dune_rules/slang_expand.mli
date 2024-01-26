open! Stdune
open Import

type expander =
  String_with_vars.t
  -> dir:Path.t
  -> (Value.t list, [ `Undefined_pkg_var of Dune_lang.Package_variable_name.t ]) result
       Memo.t

val eval_multi_located
  :  Slang.t list
  -> dir:Path.t
  -> f:expander
  -> (Loc.t * [ `Concat of Value.t list ]) list Memo.t

val eval_blang : Slang.blang -> dir:Path.t -> f:expander -> bool Memo.t
