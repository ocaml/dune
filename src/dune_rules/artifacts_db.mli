(* This module is separate from [Artifacts] to avoid cycles *)

val get : Context.t -> Artifacts.t Memo.t
