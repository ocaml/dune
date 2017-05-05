(** Generate a META file *)

open! Import

val gen
  :  package:string
  -> version:(unit, string option) Build.t
  -> stanzas:(Path.t * Jbuild_types.Stanza.t) list
  -> lib_deps:(dir:Path.t
               -> Jbuild_types.Stanza.t
               -> (Meta.entry list, string list) Build.t)
  -> ppx_runtime_deps:(dir:Path.t
                       -> Jbuild_types.Stanza.t
                       -> (Meta.entry list, string list) Build.t)
  -> (unit, Meta.t) Build.t
