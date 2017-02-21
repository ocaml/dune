open! Import
val setup
  :  ?filter_out_optional_stanzas_with_missing_deps:bool
  -> unit
  -> (Build_system.t * (Path.t * Jbuild_types.Stanza.t list) list * Context.t)
       Future.t
val external_lib_deps : packages:string list -> Build.lib_deps Path.Map.t
val report_error : ?map_fname:(string -> string) -> Format.formatter -> exn -> unit

val bootstrap : unit -> unit
