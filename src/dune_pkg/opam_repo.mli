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

(** [of_opam_repo_dir_path opam_repo_dir] creates a repo represented by a local
    directory in the path given by [opam_repo_dir]. *)
val of_opam_repo_dir_path : Loc.t -> Path.t -> t

(** [of_git_repo git source] loads the opam repository located
    at [source] from git. [source] can be any URL that [git remote add]
    supports. *)
val of_git_repo : Loc.t -> OpamUrl.t -> t Fiber.t

val revision : t -> Rev_store.At_rev.t
val serializable : t -> Serializable.t option

(** Load package metadata for all versions of a package with a given name *)
val load_all_versions
  :  t list
  -> OpamPackage.Name.t
  -> Resolved_package.t OpamPackage.Version.Map.t Fiber.t

module Private : sig
  val create : source:string option -> t
end
