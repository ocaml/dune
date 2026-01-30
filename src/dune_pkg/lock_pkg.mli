open Dune_opam
open Stdune

(** Convert a selected opam package to a package that dune can save to the lock
    directory *)
val opam_package_to_lock_file_pkg
  :  Solver_env.t
  -> Solver_stats.Updater.t
  -> Package_version.t Package_name.Map.t
  -> OpamPackage.t
  -> pinned:bool
  -> Resolved_package.t
  -> portable_lock_dir:bool
  -> (Lock_dir.Pkg.t, User_message.t) result
