open! Import
open Jbuild

type setup =
  { build_system : Build_system.t
  ; (* Evaluated jbuilds per context names *)
    stanzas      : (Path.t * Scope_info.t * Stanzas.t) list String_map.t
  ; contexts     : Context.t list
  ; packages     : Package.t Package.Name.Map.t
  ; file_tree    : File_tree.t
  }

(* Returns [Error ()] if [pkg] is unknown *)
val package_install_file : setup -> Package.Name.t -> (Path.t, unit) result

(** Scan the source tree and discover everything that's needed in order to build
    it. *)
val setup
  :  ?log:Log.t
  -> ?filter_out_optional_stanzas_with_missing_deps:bool
  -> ?workspace:Workspace.t
  -> ?workspace_file:string
  -> ?only_packages:Package.Name.Set.t
  -> ?x:string
  -> ?ignore_promoted_rules:bool
  -> unit
  -> setup Fiber.t
val external_lib_deps
  : ?log:Log.t
  -> packages:Package.Name.t list
  -> unit
  -> Build.lib_deps Path.Map.t

val find_context_exn : setup -> name:string -> Context.t

(**/**)

(* This is used to bootstrap jbuilder itself. It is not part of the public API. *)
val bootstrap : unit -> unit
