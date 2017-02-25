open! Import

type setup =
  { build_system   : Build_system.t
  ; stanzas        : (Path.t * Jbuild_types.Stanza.t list) list
  ; context        : Context.t
  ; packages       : Package.t String_map.t
  }

(* Returns [Error ()] if [pkg] is unknown *)
val package_install_file : setup -> string -> (Path.t, unit) result

val setup
  :  ?filter_out_optional_stanzas_with_missing_deps:bool
  -> unit
  -> setup Future.t
val external_lib_deps
  : ?log:out_channel
  -> packages:string list
  -> unit
  -> Build.lib_deps Path.Map.t
val report_error : ?map_fname:(string -> string) -> Format.formatter -> exn -> unit

val bootstrap : unit -> unit

val create_log : unit -> out_channel
