(** Allows defining data per context without initializing the entire context *)

open Import

val profile : Context_name.t -> Profile.t Memo.t
val valid : Context_name.t -> bool Memo.t
val list : unit -> Context_name.t list Memo.t
