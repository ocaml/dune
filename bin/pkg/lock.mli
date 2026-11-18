open Import

(** Collect all pins from all projects in the workspace. *)
val project_pins : Dune_pkg.Pin.DB.t Memo.t

module Dependency_flags_arg : sig
  (** The "--with-test", "--with-doc" and "--with-dev-setup" options, which
      override solver variables of the workspace. *)
  type t

  (** Don't override any variable. *)
  val none : t
end

val solve
  :  Workspace.t
  -> local_packages:Dune_pkg.Local_package.t Package_name.Map.t
  -> project_pins:Dune_pkg.Pin.DB.t
  -> solver_env_from_current_system:Dune_pkg.Solver_env.t option
  -> version_preference:Dune_pkg.Version_preference.t option
  -> lock_dirs:Path.t list
  -> print_perf_stats:bool
  -> portable_lock_dir:bool
  -> dependency_flags:Dependency_flags_arg.t
  -> unit Fiber.t

(** Command to create lock directory *)
val command : unit Cmd.t
