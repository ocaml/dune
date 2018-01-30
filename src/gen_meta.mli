(** Generate a META file *)

open! Import

val gen
  :  package:string
  -> scope:Lib_db.Scope.t
  -> version:string option
  -> stanzas:(Path.t * Jbuild.Stanza.t) list
  -> Meta.t
