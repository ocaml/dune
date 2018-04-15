open! Import
open Jbuild

(* Generate rules. Returns evaluated jbuilds per context names. *)
val gen
  :  contexts:Context.t list
  -> build_system:Build_system.t
  -> ?external_lib_deps_mode:bool (* default: false *)
  -> ?only_packages:Package.Name.Set.t
  -> Jbuild_load.conf
  -> (Path.t * Scope_info.t * Stanzas.t) list String_map.t Fiber.t
