open Stdune

type t

module Object : sig
  (** A git object that can exist in storage *)
  type t

  (** An object that definitely exists in the storage, but might still not be
      fetched *)
  type resolved = private t

  val of_sha1 : string -> t option
  val to_string : t -> string
  val equal : t -> t -> bool
  val to_dyn : t -> Dyn.t
end

module File : sig
  type t

  val path : t -> Path.Local.t
  val to_dyn : t -> Dyn.t

  module Set : Set.S with type elt = t
end

module At_rev : sig
  type t

  module Config : sig
    val parse : string -> (string * string option * string * string) option
  end

  val rev : t -> Object.t
  val content : t -> Path.Local.t -> string option Fiber.t
  val directory_entries : t -> recursive:bool -> Path.Local.t -> File.Set.t
  val equal : t -> t -> bool
  val check_out : t -> target:Path.t -> unit Fiber.t
end

module Remote : sig
  (** handle representing a particular git repository *)
  type t

  val default_branch : t -> Object.resolved option Fiber.t
end

val remote : t -> url:Loc.t * string -> Remote.t
val resolve_revision : t -> Remote.t -> revision:string -> Object.resolved option Fiber.t
val content_of_files : t -> File.t list -> string list Fiber.t
val load_or_create : dir:Path.t -> t Fiber.t
val get : t Fiber.t
val fetch_object : t -> Remote.t -> Object.t -> At_rev.t option Fiber.t
val fetch_resolved : t -> Remote.t -> Object.resolved -> At_rev.t Fiber.t
