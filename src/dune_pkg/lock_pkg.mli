(** Add values for expanding [%{name}] for a package *)
val add_self_to_filter_env
  :  OpamPackage.t
  -> (OpamTypes.full_variable -> OpamVariable.variable_contents option)
  -> OpamTypes.full_variable
  -> OpamVariable.variable_contents option

(** Convert a selected opam package to a package that dune can save to the lock
    directory *)
val opam_package_to_lock_file_pkg
  :  Solver_env.t
  -> Solver_stats.Updater.t
  -> Package_version.t Package_name.Map.t
  -> OpamPackage.t
  -> pinned_package_names:Package_name.Set.t
  -> candidates_cache:(Package_name.t -> Resolved_package.t OpamPackage.Version.Map.t)
  -> [> `Compiler | `Non_compiler ] * Lock_dir.Pkg.t
