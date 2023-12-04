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
  (** [uninit] represents an uninitialized remote. Use [update] or
      [don't_update] to get a remote [t] *)
  type uninit

  (** handle representing a particular git repository *)
  type t

  val equal : t -> t -> bool

  (** [update remote] will fetch the most current revisions from the remote *)
  val update : uninit -> t Fiber.t

  (** [don't_update] signals that the remote should not be updated *)
  val don't_update : uninit -> t

  val default_branch : t -> string
  val rev_of_name : t -> name:string -> At_rev.t option Fiber.t
  val rev_of_repository_id : t -> Repository_id.t -> At_rev.t option Fiber.t
end

val content_of_files : t -> File.t list -> string list Fiber.t
val load_or_create : dir:Path.t -> t Fiber.t

(** [add_repo t ~source] idempotently registers a git repo to the rev store.
    [source] is any URL that is supported by [git remote add].

    This only adds the remote metadata, to get a remote you need to either
    use [Remote.update] if you want to fetch from the remote (thus potentially
    triggering network IO) or if you are sure the [t] already contains all
    required revisions (e.g. from a previous run) then use [don't_update]. *)
val add_repo : t -> source:string -> Remote.uninit Fiber.t
