open Stdune

(** Digests (MD5) *)

type t

include Comparable_intf.S with type key := t

val to_dyn : t -> Dyn.t

val hash : t -> int

val equal : t -> t -> bool

val compare : t -> t -> Ordering.t

val to_string : t -> string

val from_hex : string -> t option

val file : Path.t -> t

val string : string -> t

val to_string_raw : t -> string

val generic : 'a -> t

(** The subset of fields of [Unix.stats] used by this module.

    By requiring only a subset of fields here, we allow the caller to memoize
    only the fields that really matter. *)
module Stats_for_digest : sig
  type t =
    { st_kind : Unix.file_kind
    ; st_perm : Unix.file_perm
    }

  val of_unix_stats : Unix.stats -> t
end

module Path_digest_result : sig
  type nonrec t =
    | Ok of t
    | Unexpected_kind
    | Unix_error of Dune_filesystem_stubs.Unix_error.Detailed.t
        (** A Unix error, e.g., [(ENOENT, _, _)] if the path doesn't exist. *)

  val equal : t -> t -> bool
end

(** Digest a path taking into account its [Stats_for_digest].

    - If it's a regular file, the resulting digest includes the file's content
      as well as the its executable permissions bit.

    - If it's a directory and [allow_dirs = true], the function computes the
      digest of all contained filename/digest pairs, recursively.

    - Otherwise, the function returns [Unexpected_kind].

    Note that this interface is prone to races: the provided [Stats_for_digest]
    may get stale, so [path_with_stats] may return [Unix_error (ENOENT, _, _)]
    even though you've just successfully run [Path.stat] on it. The call sites
    are expected to gracefully handle such races. *)
val path_with_stats :
  allow_dirs:bool -> Path.t -> Stats_for_digest.t -> Path_digest_result.t

(** Digest a file taking the [executable] bit into account. Should not be called
    on a directory. *)
val file_with_executable_bit : executable:bool -> Path.t -> t

(** Override the implementations of digest computation. Can be used to record
    the reverse digest map. *)
val override_impl : file:(string -> t) -> string:(string -> t) -> unit

(** [Direct_impl] does a plain hashing, with no heed to the overrides given by
    [override_impl]. *)
module Direct_impl : sig
  val string : string -> t
end
