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

(** [of_git_repo git ~repo_id ~update ~source] loads the opam repository located at [source] from git.
    [source] can be any URL that [git remote add] supports.

    Set [update] to true to update the source to the newest revision, otherwise it will use the latest
    data available in the cache (if any). *)
val of_git_repo
  :  repo_id:Repository_id.t option
  -> update:bool
  -> source:string
  -> t Fiber.t

val repo_id : t -> Repository_id.t option
val source : t -> string option
val serializable : t -> Serializable.t option

module With_file : sig
  type repo := t
  type t

  val package : t -> OpamPackage.t
  val opam_file : t -> OpamFile.OPAM.t
  val file : t -> Path.t
  val repo : t -> repo
end

(** Load package metadata for all versions of a package with a given name *)
val load_all_versions
  :  t list
  -> OpamPackage.Name.t
  -> With_file.t OpamPackage.Version.Map.t Fiber.t

val get_opam_package_files : With_file.t list -> File_entry.t list list Fiber.t

module Private : sig
  val create : source:string option -> repo_id:Repository_id.t option -> t
end
