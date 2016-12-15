(** Generate a META file *)

open! Import

val gen
  :  package:string
  -> stanzas:(Path.t * Jbuild_types.Stanza.t list) list
  -> lib_deps:(dir:Path.t
               -> Jbuild_types.Stanza.t
               -> ('a, string list) Build.t)
  -> ppx_runtime_deps:(dir:Path.t
                       -> Jbuild_types.Stanza.t
                       -> ('a, string list) Build.t)
  -> ('a, Meta.t) Build.t
