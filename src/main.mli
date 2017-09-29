open! Import
open Jbuild

type setup =
  { build_system : Build_system.t
  ; (* Evaluated jbuilds per context names *)
    stanzas      : (Path.t * Scope.t * Stanzas.t) list String_map.t
  ; contexts     : Context.t list
  ; packages     : Package.t String_map.t
  ; file_tree    : File_tree.t
  }

(* Returns [Error ()] if [pkg] is unknown *)
val package_install_file : setup -> string -> (Path.t, unit) result

val setup
  :  ?log:Log.t
  -> ?filter_out_optional_stanzas_with_missing_deps:bool
  -> ?workspace:Workspace.t
  -> ?workspace_file:string
  -> ?only_packages:String_set.t
  -> unit
  -> setup Future.t
val external_lib_deps
  : ?log:Log.t
  -> packages:string list
  -> unit
  -> Build.lib_deps Path.Map.t
val report_error : ?map_fname:(string -> string) -> Format.formatter -> exn -> unit

val bootstrap : unit -> unit

val find_context_exn : setup -> name:string -> Context.t
