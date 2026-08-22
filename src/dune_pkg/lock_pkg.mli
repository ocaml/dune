open Stdune

(** Add values for expanding [%{name}] for a package *)
val add_self_to_filter_env
  :  OpamPackage.t
  -> (OpamTypes.full_variable -> OpamVariable.variable_contents option)
  -> OpamTypes.full_variable
  -> OpamVariable.variable_contents option

(** Evaluate a local package's dependency formula. This is the single
    implementation shared by the solver and by validation
    ([Package_universe]), so the two cannot drift apart. The package's own
    [name] and [version] are bound as self variables and the running version
    of Dune is injected into [packages]. Only regular (non-post) dependencies
    are returned. Dune is removed from the returned list because it is a
    pseudo-package that is not written to lockdirs. [env] is the filter
    environment before the self bindings are added. *)
val local_package_dependencies
  :  Local_package.For_solver.t
  -> env:OpamFilter.env
  -> with_test:bool
  -> packages:Package_version.t Package_name.Map.t
  -> dune_version:Package_version.t
  -> (Package_name.t list, Resolve_opam_formula.unsatisfied_formula) result

(** Convert a selected opam package into one lock-directory package branch per
    solver environment. Each solver environment is paired with the packages
    selected on that platform. The caller can discard unreachable branches
    before merging the remaining platform-specific fields. *)
val opam_package_to_lock_file_pkg_branches
  :  (Solver_env.t * Package_version.t Package_name.Map.t) list
  -> Solver_stats.Updater.t
  -> OpamPackage.t
  -> pinned:bool
  -> Resolved_package.t
  -> ((Solver_env.t * Lock_dir.Pkg.t) list, User_message.t) result
