open! Dune_engine
open! Stdune
open! Import

type build_system =
  { conf : Dune_load.conf
  ; contexts : Context.t list
  ; scontexts : Super_context.t Context_name.Map.t
  }

(** Load dune files and initializes the build system *)
val init_build_system :
     stats:Stats.t option
  -> only_packages:Package.t Package.Name.Map.t option
  -> sandboxing_preference:Sandbox_mode.t list
  -> caching:Build_system.caching option
  -> build_mutex:Fiber.Mutex.t option
  -> conf:Dune_load.conf
  -> contexts:Context.t list
  -> build_system Fiber.t

val find_context_exn : build_system -> name:Context_name.t -> Context.t

val find_scontext_exn : build_system -> name:Context_name.t -> Super_context.t
