open! Import

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

(** Verifies if the dependencies described in the project file are still
    synchronized with the dependencies selected in the lock directroy. If it is
    not the case, it returns the hash of the new dependency set. *)
val up_to_date
  :  Local_package.t Package_name.Map.t
  -> dependency_hash:Local_package.Dependency_hash.t option
  -> [ `Valid | `Invalid ]

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
