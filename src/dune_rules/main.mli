open! Dune_engine
open! Stdune
open! Import

type workspace =
  { contexts : Context.t list
  ; conf : Dune_load.conf
  ; env : Env.t
  }

type build_system =
  { workspace : workspace
  ; scontexts : Super_context.t Context_name.Map.t
  }

(* Returns [Error ()] if [pkg] is unknown *)
val package_install_file :
  workspace -> Package.Name.t -> (Path.Source.t, unit) result

(** Scan the source tree and discover the overall layout of the workspace. *)
val scan_workspace : capture_outputs:bool -> unit -> workspace Fiber.t

(** Load dune files and initializes the build system *)
val init_build_system :
     ?stats:Stats.t
  -> ?only_packages:Package.t Package.Name.Map.t
  -> sandboxing_preference:Sandbox_mode.t list
  -> ?caching:Build_system.caching
  -> ?build_mutex:Fiber.Mutex.t
  -> workspace
  -> build_system Fiber.t

val find_context_exn : workspace -> name:Context_name.t -> Context.t

val find_scontext_exn : build_system -> name:Context_name.t -> Super_context.t

(** Setup the environment *)
val setup_env : capture_outputs:bool -> Env.t Fiber.t
