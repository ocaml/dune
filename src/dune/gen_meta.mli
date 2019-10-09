(** Generate a META file *)

open! Import

val gen :
     package:string
  -> version:string option
  -> ?add_directory_entry:bool
  -> Super_context.Lib_entry.t list
  -> Meta.t
