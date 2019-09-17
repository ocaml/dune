(** Generate a META file *)

open! Import

val gen :
     package:string
  -> version:string option
  -> Super_context.Lib_entry.t list
  -> Meta.t
