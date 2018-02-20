(** Generate a META file *)

open! Import

val gen
  :  package:string
  -> version:string option
  -> meta_path:Path.t
  -> Lib.t list
  -> Meta.t
