(** Evaluate package availability without constructing a build context. *)

open Import

val eval : Package.t -> bool Memo.t
