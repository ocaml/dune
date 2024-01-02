open Import

type t

module Serializable : sig
  type t

  val encode : t -> Dune_lang.t list
  val decode : t Decoder.t
  val equal : t -> t -> bool
  val to_dyn : t -> Dyn.t

  module Private : sig
    val with_commit : commit:string -> t -> t
  end
end

val equal : t -> t -> bool

module Source : sig
  type commitish =
    | Commit of string
    | Branch of string
    | Tag of string

  type t =
    { url : string
    ; commit : commitish option
    }

  val of_opam_url : OpamUrl.t -> t Fiber.t
  val to_string : t -> string
  val to_dyn : t -> Dyn.t
  val equal : t -> t -> bool

  module Private : sig
    val of_opam_url : Rev_store.t -> OpamUrl.t -> t Fiber.t
  end
end

(** [of_opam_repo_dir_path opam_repo_dir] creates a repo represented by a local
    directory in the path given by [opam_repo_dir]. *)
val of_opam_repo_dir_path
  :  source:string option
  -> repo_id:Repository_id.t option
  -> Path.t
  -> t

(** [of_git_repo git ~repo_id ~update source] loads the opam repository located at [source] from git.
    [source] can be any URL that [git remote add] supports.

    Set [update] to true to update the source to the newest revision, otherwise it will use the latest
    data available in the cache (if any). *)
val of_git_repo : repo_id:Repository_id.t option -> update:bool -> Source.t -> t Fiber.t

val repo_id : t -> Repository_id.t option
val serializable : t -> Serializable.t option

module With_file : sig
  type t

  val package : t -> OpamPackage.t
  val opam_file : t -> OpamFile.OPAM.t
  val file : t -> Path.t
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
