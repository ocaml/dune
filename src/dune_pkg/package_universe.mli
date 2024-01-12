open! Import

(** All of the packages in a dune project including local packages and packages
    in a lockdir. The lockdir is guaranteed to contain a valid dependency
    solution for the local packages. *)
type t

val create
  :  Local_package.t Opam_compatible_package_name.Map.t
  -> Lock_dir.t
  -> (t, User_message.t) result

(** Returns the dependencies of the specified package within teh package
    universe *)
val opam_package_dependencies_of_package
  :  t
  -> Opam_compatible_package_name.t
  -> which:[ `All | `Non_test | `Test_only ]
  -> traverse:[ `Immediate | `Transitive ]
  -> OpamPackage.t list

(** Returns the opam package whose name is the given package name and whose
    version is the version of that package within the package universe *)
val opam_package_of_package : t -> Opam_compatible_package_name.t -> OpamPackage.t
