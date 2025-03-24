(** Allows defining data per context without initializing the entire context *)

open Import

type 'a t = Context_name.t -> 'a Memo.t

val create_by_name
  :  name:string
  -> (Context_name.t -> 'a Memo.t)
  -> (Context_name.t -> 'a Memo.t) Staged.t

val profile : Context_name.t -> Profile.t Memo.t
val valid : Context_name.t -> bool Memo.t
val list : unit -> Context_name.t list Memo.t
