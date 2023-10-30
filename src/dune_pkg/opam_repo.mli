open Import

type t

module Serializable : sig
  type t

  val encode : t -> Dune_lang.t list
  val decode : t Decoder.t
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

(** [of_git_repo git source] loads the data through git *)
val of_git_repo : repo_id:Repository_id.t option -> source:string -> t Fiber.t

val repo_id : t -> Repository_id.t option
val source : t -> string option
val serializable : t -> Serializable.t option

module With_file : sig
  type t =
    { opam_file : OpamFile.OPAM.t
    ; file : Path.t
    }
end

(** Load package metadata for a single package *)
val load_opam_package : t -> OpamPackage.t -> With_file.t option Fiber.t

(** Load package metadata for all versions of a package with a given name *)
val load_all_versions : t list -> OpamPackage.Name.t -> OpamFile.OPAM.t list Fiber.t

val get_opam_package_files : t -> OpamPackage.t -> File_entry.t list Fiber.t

module Private : sig
  val create : source:string option -> repo_id:Repository_id.t option -> t
end
