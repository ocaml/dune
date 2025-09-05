open Import

val solve
  :  Workspace.t
  -> local_packages:Dune_pkg.Local_package.t Package_name.Map.t
  -> project_pins:Dune_pkg.Pin.DB.t
  -> solver_env_from_current_system:Dune_pkg.Solver_env.t option
  -> version_preference:Dune_pkg.Version_preference.t option
  -> lock_dirs:Path.t list
  -> print_perf_stats:bool
  -> portable_lock_dir:bool
  -> unit Fiber.t

(** Command to create lock directory *)
val command : unit Cmd.t
