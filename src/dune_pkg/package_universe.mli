open! Import

(** All of the packages in a dune project including local packages and packages
    in a lockdir. The lockdir is guaranteed to contain a valid dependency
    solution for the local packages. *)
type t

val create
  :  Local_package.t Package_name.Map.t
  -> Lock_dir.t
  -> (t, User_message.t) result

(** Verifies if the dependencies described in the project file are still
    synchronize with the dependencies selected in the lock directroy. If it is
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
