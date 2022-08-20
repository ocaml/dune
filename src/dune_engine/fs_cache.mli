open Import

(** A cached file-system operation on a [Path.Outside_build_dir.t] whose result
    type is ['a]. For example, an operation to check if a path exists returns
    ['a = bool].

    Currently we do not expose a way to construct such cached operations; see
    the [Untracked] module for a few predefined ones. *)
type 'a t

(** If the cache contains the result of applying an operation to a path, return
    it. Otherwise, perform the operation, store the result in the cache, and
    then return it. *)
val read : 'a t -> Path.Outside_build_dir.t -> 'a

(** Evict an entry from the cache. *)
val evict : 'a t -> Path.Outside_build_dir.t -> unit

(** Result of updating a cache entry. *)
module Update_result : sig
  type t =
    | Skipped  (** No need to update a given entry because it has no readers *)
    | Updated of { changed : bool }

  (** [Skipped] is the "empty" update. *)
  val empty : t

  val combine : t -> t -> t

  val to_dyn : t -> Dyn.t
end

(** Perform an operation and update the result stored in the cache. *)
val update : 'a t -> Path.Outside_build_dir.t -> Update_result.t

(** This module caches only a subset of fields of [Unix.stats] because other
    fields are currently unused.

    Note that we specifically do not want to cache [mtime] and [ctime] because
    these fields can change too often: for example, when a temporary file is
    created in a watched directory. *)
module Reduced_stats : sig
  type t =
    { st_dev : int  (** Device number *)
    ; st_ino : int  (** Inode number *)
    ; st_kind : File_kind.t  (** Kind of the file *)
    }

  val of_unix_stats : Unix.stats -> t

  val equal : t -> t -> bool
end

module Dir_contents : sig
  type t

  (** The sorted list of file names with kinds. *)
  val to_list : t -> (string * File_kind.t) list

  val iter : t -> f:(string * File_kind.t -> unit) -> unit

  val equal : t -> t -> bool
end

(** A few predefined cached operations. They are "untracked" in the sense that
    the user is responsible for tracking the file system and manually calling
    the [update] function to bring the stored results up to date.

    See [fs_memo.ml] for tracked versions of these operations. *)
module Untracked : sig
  val path_stat : (Reduced_stats.t, Unix_error.Detailed.t) result t

  val file_digest : Cached_digest.Digest_result.t t

  val dir_contents : (Dir_contents.t, Unix_error.Detailed.t) result t
end

module Debug : sig
  (** The name of a cached operation. *)
  val name : 'a t -> string
end
