(** Generate a META file *)

open! Import

type package_version =
  | This of string
  | Load of Path.t
  | Na

val gen
  :  package:string
  -> version:package_version
  -> stanzas:(Path.t * Jbuild_types.Stanza.t) list
  -> lib_deps:(dir:Path.t
               -> Jbuild_types.Stanza.t
               -> (Meta.entry list, string list) Build.t)
  -> ppx_runtime_deps:(dir:Path.t
                       -> Jbuild_types.Stanza.t
                       -> (Meta.entry list, string list) Build.t)
  -> (unit, Meta.t) Build.t
