open! Stdune
open! Import

type workspace =
  { contexts : Context.t list
  ; conf     : Dune_load.conf
  ; env      : Env.t
  }

type build_system =
  { workspace    : workspace
  ; scontexts    : Super_context.t String.Map.t
  }

(* Returns [Error ()] if [pkg] is unknown *)
val package_install_file : workspace -> Package.Name.t -> (Path.Source.t, unit) result

(** Scan the source tree and discover the overall layout of the workspace. *)
val scan_workspace
  :  ?log:Log.t
  -> ?workspace:Workspace.t
  -> ?workspace_file:Path.t
  -> ?x:string
  -> ?capture_outputs:bool
  -> ?profile:string
  -> ancestor_vcs:Vcs.t option
  -> unit
  -> workspace Fiber.t

(** Load dune files and initializes the build system *)
val init_build_system
  :  ?only_packages:Package.Name.Set.t
  -> ?external_lib_deps_mode:bool
  -> workspace
  -> build_system Fiber.t

val find_context_exn : workspace -> name:string -> Context.t

(** Setup the environment *)
val setup_env : capture_outputs:bool -> Env.t

(** Set the concurrency level according to the user configuration *)
val set_concurrency : ?log:Log.t -> Config.t -> unit Fiber.t

(**/**)

(* This is used to bootstrap dune itself. It is not part of the public API. *)
val bootstrap : unit -> unit
