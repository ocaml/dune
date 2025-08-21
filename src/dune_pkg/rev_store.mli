open Stdune

type t

(** Get the revision store and initialise if it hasn't been already. *)
val get : t Fiber.t

val to_dyn : t -> Dyn.t

module Object : sig
  (** A git object that can exist in storage *)
  type t

  (** An object that definitely exists in the storage, but might still not be
      fetched *)
  type resolved = private t

  val of_sha1 : string -> t option
  val to_hex : t -> string
end

module File : sig
  (** A file in the revision store. *)
  type t

  val path : t -> Path.Local.t
  val to_dyn : t -> Dyn.t

  module Set : Set.S with type elt = t
end

(** Fetch the content of many files at once from the revision store. *)
val content_of_files : t -> File.t list -> string list Fiber.t

module Remote : sig
  (** A git remote repository. *)
  type t

  (** The [default_branch] of a remote repository. *)
  val default_branch : t -> Object.resolved option Fiber.t
end

(** [remote t ~loc ~url] gets the remote pointed to by [url] and makes
    the revision store aware of it. *)
val remote : t -> loc:Loc.t -> url:string -> Remote.t

module At_rev : sig
  (** A git repository at a particular revision. *)
  type t

  val equal : t -> t -> bool
  val rev : t -> Object.t
  val content : t -> Path.Local.t -> string option Fiber.t

  (** Get the files of a git repository at a particular revision. *)
  val directory_entries : t -> recursive:bool -> Path.Local.t -> File.Set.t

  (** Get the contents of a git repository at a paricular revision and unpack
      them in the [target] directory. *)
  val check_out : t -> target:Path.t -> unit Fiber.t

  (** For testing only. *)
  module Config : sig
    val parse : string -> (string * string option * string * string) option
  end
end

(** Fetch an object at a given revision from a remote. *)
val fetch_object : t -> Remote.t -> Object.t -> At_rev.t option Fiber.t

(** Attempt to resolve the revision in the given remote. *)
val resolve_revision : t -> Remote.t -> revision:string -> Object.resolved option Fiber.t

(** Fetch the contents of the repository at the given revision into the store. *)
val fetch_resolved : t -> Remote.t -> Object.resolved -> At_rev.t Fiber.t

module Debug : sig
  val files_and_submodules_cache : bool ref
  val content_of_files_cache : bool ref
end
