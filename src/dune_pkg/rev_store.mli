open Stdune

(** * The Revision Store

The revision store is a single shared bare Git repository acting as a cache for
packages and package repositories.

Git is used as a content-addressable file system. File content of both package
repositories and opam repositories live here. Having a single place
is advantageous because it saves space.

Dune is able to lookup any stored Git repository by its revision and see all
the files that exist within. Those files can then have their contents
fetched. *)

(** Abstract handle to the shared revision store. *)
type t

val to_dyn : t -> Dyn.t

(** Get the revision store and initialise if it hasn't been already. *)
val get : t Fiber.t

module Object : sig
  (** A git object that can exist in storage. *)
  type t

  (** An object that definitely exists in the storage, but might still not be
      fetched *)
  type resolved = private t

  val of_sha1 : string -> t option
  val to_hex : t -> string
end

module File : sig
  (** A file entry at a particular revision. May be a direct blob or a redirect
      into a submodules file. *)
  type t

  val to_dyn : t -> Dyn.t

  (** Path relative to the repository root at the given revision. *)
  val path : t -> Path.Local.t

  module Set : Set.S with type elt = t
end

(** [content_of_files t files] fetches the content of a list of [files] in one
    batch. It returns a list of strings consisting of the content of the
    corresponding files. *)
val content_of_files : t -> File.t list -> string list Fiber.t

module Remote : sig
  (** A git remote repository known to the revision store. *)
  type t

  val to_dyn : t -> Dyn.t

  (** The [default_branch] of a remote repository. *)
  val default_branch : t -> Object.resolved option Fiber.t
end

(** [remote ~loc ~url] gets the remote pointed to by [url] and makes
    the revision store aware of it. This is idempotent and is memoized by
    [url]. *)
val remote : t -> loc:Loc.t -> url:string -> Remote.t

module At_rev : sig
  (** An immutable Git repository view at a specific commit. *)
  type t

  val equal : t -> t -> bool

  (** The underlying commit. *)
  val rev : t -> Object.t

  (** Read file content at this revision. Returns [None] if the file does not
      exist or is not a blob. *)
  val content : t -> Path.Local.t -> string option Fiber.t

  (** [directory_entries t ~recursive path] lists the files under [path]. If
      [recursive] then lists the full subtree. Symbolic links are returned as
      files. *)
  val directory_entries : t -> recursive:bool -> Path.Local.t -> File.Set.t

  (** [check_out t ~target] materialises the entire tree into [target]
      including any submodules. *)
  val check_out : t -> target:Path.t -> unit Fiber.t

  (** For testing only. *)
  module Config : sig
    val parse : string -> (string * string option * string * string) option
  end
end

(** Resolve the revision in the given remote. The [revision] can be any
    accepted reference name, branch, tag or Sha1 by Git.

    If a reference name is resolved form the remote, the commit is fetched. If
    the revision is already present, no network I/O is performed. Returns
    [None] if the remote reports "not found". Raises a user error if the
    reference is ambigious. *)
val resolve_revision : t -> Remote.t -> revision:string -> Object.resolved option Fiber.t

(** [fetch_object t remote object] ensures that an [object] from the [remote]
    is present in the revision store [t]. If the revision is already present,
    no network I/O is performed. Returns [Error git_error_lines] if the remote
    reports "not found". *)
val fetch_object : t -> Remote.t -> Object.t -> (At_rev.t, string list) result Fiber.t

(** Fetch the file contents of the repository at the given revision into the
    store and return the repository view. *)
val fetch_resolved : t -> Remote.t -> Object.resolved -> At_rev.t Fiber.t

module Debug : sig
  val files_and_submodules_cache : bool ref
  val content_of_files_cache : bool ref
end
