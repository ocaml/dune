open! Import

val gen
  :  contexts:Context.t list
  -> ?filter_out_optional_stanzas_with_missing_deps:bool (** default: true *)
  -> ?only_package:string
  -> Jbuild_load.conf
  -> (Build_interpret.Rule.t list *
     (* Evaluated jbuilds per context names *)
     (Path.t * Jbuild_types.Stanzas.t) list String_map.t) Future.t
