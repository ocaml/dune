open Import

val action
  :  target:Path.Build.t
  -> lock_dir:Path.t
  -> packages:Dune_pkg.Local_package.t Package.Name.Map.t
  -> repos:Dune_pkg.Opam_repo.t list
  -> solver_env_from_context:Dune_pkg.Solver_env.t
  -> unset_solver_vars:Package_variable_name.Set.t
  -> constraints:Dune_pkg.Package_dependency.t list
  -> selected_depopts:Dune_pkg.Package_name.t list
  -> pins:Dune_pkg.Resolved_package.t Dune_lang.Package_name.Map.t
  -> version_preference:Dune_pkg.Version_preference.t
  -> Action.t
