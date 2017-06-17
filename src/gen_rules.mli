open! Import
open Jbuild

val gen
  :  contexts:Context.t list
  -> ?filter_out_optional_stanzas_with_missing_deps:bool (* default: true *)
  -> ?only_packages:String_set.t
  -> Jbuild_load.conf
  -> (Build_interpret.Rule.t list *
     (* Evaluated jbuilds per context names *)
     (Path.t * Scope.t * Stanzas.t) list String_map.t) Future.t
