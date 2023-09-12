(** Allows defining data per context without initializing the entire context *)

open Import

val profile : Context_name.t -> Profile.t Memo.t
