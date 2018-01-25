(** Generate a META file *)

open! Import

val gen
  :  package:string
  -> version:string option
  -> stanzas:(Path.t * Jbuild.Stanza.t) list
  -> resolve_lib_dep_names:(dir:Path.t -> Jbuild.Lib_dep.t list -> string list)
  -> all_ppx_runtime_deps_exn:
       (dir:Path.t -> Jbuild.Lib_dep.t list -> String_set.t)
  -> Meta.t
