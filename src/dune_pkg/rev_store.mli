open Stdune

type t

module File : sig
  type t

  val path : t -> Path.Local.t
  val to_dyn : t -> Dyn.t

  module Set : Set.S with type elt = t
end

module At_rev : sig
  type t

  val content : t -> Path.Local.t -> string option Fiber.t
  val directory_entries : t -> Path.Local.t -> File.Set.t
  val equal : t -> t -> bool
  val repository_id : t -> Repository_id.t
end

module Remote : sig
  type t

  val equal : t -> t -> bool
  val update : t -> unit Fiber.t
  val default_branch : t -> string option Fiber.t
  val rev_of_name : t -> name:string -> At_rev.t option Fiber.t
  val rev_of_repository_id : t -> Repository_id.t -> At_rev.t option Fiber.t
end

val content_of_files : t -> File.t list -> string list Fiber.t
val load_or_create : dir:Path.t -> t Fiber.t
val add_repo : t -> source:string -> Remote.t Fiber.t
