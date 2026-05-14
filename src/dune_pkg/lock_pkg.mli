open Stdune

(** Add values for expanding [%{name}] for a package *)
val add_self_to_filter_env
  :  OpamPackage.t
  -> (OpamTypes.full_variable -> OpamVariable.variable_contents option)
  -> OpamTypes.full_variable
  -> OpamVariable.variable_contents option

(** Convert a selected opam package to a package that dune can save to the lock
    directory. For portable lockdirs, all solver_envs are used to set conditions
    on conditional fields. The first solver_env is used for evaluating filters. *)
val opam_package_to_lock_file_pkg
  :  Solver_env.t list
  -> Solver_stats.Updater.t
  -> Package_version.t Package_name.Map.t
  -> OpamPackage.t
  -> pinned:bool
  -> Resolved_package.t
  -> portable_lock_dir:bool
  -> (Lock_dir.Pkg.t, User_message.t) result
