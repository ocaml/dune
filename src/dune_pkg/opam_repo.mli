open! Stdune

type t

(** [of_opam_repo_dir_path opam_repo_dir] creates a repo representedy by a local
    directory in the path given by [opam_repo_dir]. *)
val of_opam_repo_dir_path : Path.t -> t

(** Load package metadata for a single package *)
val load_opam_package : t -> OpamPackage.t -> OpamFile.OPAM.t

(** Load package metadata for all versions of a package with a given name *)
val load_all_versions :
     t
  -> OpamPackage.Name.t
  -> (OpamFile.OPAM.t list, [ `Package_not_found ]) result
