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

module Source : sig
  module Commitish : sig
    type t =
      | Commit of string
      | Branch of string
      | Tag of string
  end

  type t

  val commit : t -> Commitish.t option
  val url : t -> string
  val of_opam_url : Loc.t -> OpamUrl.t -> t Fiber.t
  val to_dyn : t -> Dyn.t
  val equal : t -> t -> bool

  module Private : sig
    val of_opam_url : Rev_store.t -> Loc.t -> OpamUrl.t -> t Fiber.t
  end
end

(** [of_opam_repo_dir_path opam_repo_dir] creates a repo represented by a local
    directory in the path given by [opam_repo_dir]. *)
val of_opam_repo_dir_path : Path.t -> t

(** [of_git_repo git ~update source] loads the opam repository located
    at [source] from git. [source] can be any URL that [git remote add]
    supports.

    Set [update] to true to update the source to the newest revision, otherwise
    it will use the latest data available in the cache (if any). *)
val of_git_repo : update:bool -> Source.t -> t Fiber.t

val serializable : t -> Serializable.t option

(** Load package metadata for all versions of a package with a given name *)
val load_all_versions
  :  t list
  -> OpamPackage.Name.t
  -> Resolved_package.t OpamPackage.Version.Map.t Fiber.t

module Private : sig
  val create : source:string option -> t
end
