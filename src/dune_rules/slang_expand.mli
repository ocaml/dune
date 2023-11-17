open! Stdune
open Import

type expander =
  String_with_vars.t
  -> dir:Path.t
  -> (Value.t list, [ `Undefined_pkg_var of Dune_pkg.Package_variable.Name.t ]) result
       Memo.t

(** Evaluate a [Slang.t] expression. Expressions in the unfollowed branches of
    if-statements are not followed and boolean expressions are evaluated
    lazily. Undefined package variables or invalid strings in the conditions of
    if and when statements do not result in errors if they occur in unevaluated
    portions of an expression. *)
val eval : Slang.t -> dir:Path.t -> f:expander -> Value.t list Memo.t

val eval_multi_located
  :  Slang.t list
  -> dir:Path.t
  -> f:expander
  -> (Loc.t * Value.t) list Memo.t

val eval_blang : Slang.blang -> dir:Path.t -> f:expander -> bool Memo.t
