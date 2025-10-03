open Stdune

(** Digests (BLAKE3) *)

type t

module Feed : sig
  type digest := t
  type hasher

  (** Type for incrementally building up the computation of a hash. A ['a t]
      can consume a value of type ['a] and incorporate it into a hash value. *)
  type 'a t = hasher -> 'a -> unit

  (** Consume any value. The result is based on the in-memory representation of
      the value, so this is unsafe to perform on types who may have different
      in-memory representation for values which are conceptually equal, such as
      sets and maps. *)
  val generic : _ t

  val contramap : 'a t -> f:('b -> 'a) -> 'b t
  val string : string t
  val bool : bool t
  val int : int t
  val list : 'a t -> 'a list t
  val option : 'a t -> 'a option t
  val tuple2 : 'a t -> 'b t -> ('a * 'b) t
  val tuple3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

  (** Feed a digest into a hasher. *)
  val digest : digest t

  (** Compute the digest of a value given a feed for the type of that value. *)
  val compute_digest : 'a t -> 'a -> digest

  (** Takes a function which feeds values into a hasher and returns a digest of
      all the values fed in this way. *)
  val compute_digest_with_hasher : (hasher -> unit) -> digest
end

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
    ; executable : bool
    }

  val of_unix_stats : Unix.stats -> t
end

module Path_digest_error : sig
  type nonrec t =
    | Unexpected_kind
    | Unix_error of Unix_error.Detailed.t
    (** A Unix error, e.g., [(ENOENT, _, _)] if the path doesn't exist. *)
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
val path_with_stats
  :  allow_dirs:bool
  -> Path.t
  -> Stats_for_digest.t
  -> (t, Path_digest_error.t) result

(** Digest a file taking the [executable] bit into account. Should not be called
    on a directory. *)
val file_with_executable_bit : executable:bool -> Path.t -> t
