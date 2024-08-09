open Import

val solve
  :  Workspace.t
  -> local_packages:Dune_pkg.Local_package.t Package_name.Map.t
  -> project_sources:Dune_pkg.Pin_stanza.DB.t
  -> solver_env_from_current_system:Dune_pkg.Solver_env.t option
  -> version_preference:Dune_pkg.Version_preference.t option
  -> lock_dirs:Path.Source.t list
  -> unit Fiber.t

(** Command to create lock directory *)
val command : unit Cmd.t
