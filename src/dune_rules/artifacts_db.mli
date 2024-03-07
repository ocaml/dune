(* This module is separate from [Artifacts] to avoid cycles *)

open Stdune

val expander : (dir:Path.Build.t -> Expander.t Memo.t) Fdecl.t
val get : Context.t -> Artifacts.t Memo.t
