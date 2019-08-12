open! Stdune
open! Import

type workspace = {contexts: Context.t list; conf: Dune_load.conf; env: Env.t}

type build_system =
  {workspace: workspace; scontexts: Super_context.t String.Map.t}

(* Returns [Error ()] if [pkg] is unknown *)
val package_install_file :
  workspace -> Package.Name.t -> (Path.Source.t, unit) result

val scan_workspace :
     ?log:Log.t
  -> ?workspace:Workspace.t
  -> ?workspace_file:Path.t
  -> ?x:string
  -> ?capture_outputs:bool
  -> ?profile:Profile.t
  -> ancestor_vcs:Vcs.t option
  -> unit
  -> workspace Fiber.t
(** Scan the source tree and discover the overall layout of the workspace. *)

val init_build_system :
     ?only_packages:Package.Name.Set.t
  -> ?external_lib_deps_mode:bool
  -> sandboxing_preference:Sandbox_mode.t list
  -> ?memory:Dune_manager.Client.t
  -> workspace
  -> build_system Fiber.t
(** Load dune files and initializes the build system *)

val find_context_exn : workspace -> name:string -> Context.t

val setup_env : capture_outputs:bool -> Env.t
(** Setup the environment *)

val set_concurrency : ?log:Log.t -> Config.t -> unit Fiber.t
(** Set the concurrency level according to the user configuration *)

(**/**)

(* This is used to bootstrap dune itself. It is not part of the public API. *)
val bootstrap : unit -> unit
