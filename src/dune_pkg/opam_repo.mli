open! Stdune

type t

module Serializable : sig
  type t

  val encode : t -> Dune_lang.t list
  val decode : t Dune_lang.Decoder.t
  val equal : t -> t -> bool
  val to_dyn : t -> Dyn.t
end

val equal : t -> t -> bool

(** [of_opam_repo_dir_path opam_repo_dir] creates a repo representedy by a local
    directory in the path given by [opam_repo_dir]. *)
val of_opam_repo_dir_path
  :  source:string option
  -> repo_id:Repository_id.t option
  -> Path.t
  -> t

val repo_id : t -> Repository_id.t option
val serializable : t -> Serializable.t option

module With_file : sig
  type t =
    { opam_file : OpamFile.OPAM.t
    ; file : Path.t
    }
end

(** Load package metadata for a single package *)
val load_opam_package : t -> OpamPackage.t -> With_file.t option

(** Load package metadata for all versions of a package with a given name *)
val load_all_versions
  :  t list
  -> OpamPackage.Name.t
  -> (OpamFile.OPAM.t list, [ `Package_not_found ]) result

val get_opam_package_files_path : t -> OpamPackage.t -> Path.t option

module Private : sig
  val create : ?source:string -> ?repo_id:Repository_id.t -> unit -> t
end
