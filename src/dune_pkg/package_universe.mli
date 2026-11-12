open Import

(** all of the packages in a dune project under the constraints of a given
    platform, including local packages and packages in a lockdir. the lockdir
    is guaranteed to contain a valid dependency solution for the local
    packages. *)
type t

(** Create a universe for a given platform.
    TODO: Consider changing this to cover all platforms simultaneously. *)
val create
  :  platform:Solver_env.t
  -> Local_package.t Package_name.Map.t
  -> Lock_dir.t
  -> (t, User_message.t) result

(** Like [create], but doesn't check that the lockdir is the exact solution for
    the local packages, so the lockdir may contain packages which no local
    package needs. Raises if a package is both local and in the lockdir. *)
val create_unvalidated
  :  platform:Solver_env.t
  -> Local_package.t Package_name.Map.t
  -> Lock_dir.t
  -> t

(** The packages which the solver selects as dependencies of a local package:
    the packages of its dependency formula, with filters evaluated against the
    solver variables of the lockdir, and its optional dependencies present in
    the universe. Raises a [User_error] if the formula can't be satisfied from
    the universe. *)
val selected_dependencies_of_local_package
  :  t
  -> Package_name.t
  -> with_test:bool
  -> Package_name.t list

(** Reject duplicate definitions across the workspace and the lock directory. *)
val check_package_names : Lock_dir.t -> local_packages:Loc.t Package_name.Map.t -> unit

(** Resolve workspace dependencies against supplied package versions. *)
val resolve_local_dependencies
  :  Local_package.t
  -> env:Solver_env.t
  -> with_test:bool
  -> packages:Package_version.t Package_name.Map.t
  -> (Package_name.t list, User_message.Style.t Pp.t list) result

(** Check requirements and any supplied workspace metadata referenced by the
    lockdir. Hand-written lockdirs without a dependency hash remain accepted. *)
val up_to_date : Local_package.t Package_name.Map.t -> Lock_dir.t -> [ `Valid | `Invalid ]

(** Returns the dependencies of the specified package within the package
    universe *)
val opam_package_dependencies_of_package
  :  t
  -> Package_name.t
  -> which:[ `All | `Non_test | `Test_only ]
  -> traverse:[ `Immediate | `Transitive ]
  -> OpamPackage.t list

(** Returns the opam package whose name is the given package name and whose
    version is the version of that package within the package universe *)
val opam_package_of_package : t -> Package_name.t -> OpamPackage.t
