(** Digests (MD5) *)

type t

module Set : Set.S with type elt = t

module Map : Map.S with type key = t

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

(** The subset of fields of [Unix.stats] user by this module. *)
module Stats_for_digest : sig
  type t =
    { st_kind : Unix.file_kind
    ; st_perm : Unix.file_perm
    ; st_size : int
    ; st_mtime : float
    ; st_ctime : float
    }

  val of_unix_stats : Unix.stats -> t
end

module Path_digest_result : sig
  type nonrec t =
    | Ok of t
    | Not_found  (** Path does not exist *)
    | Unexpected_kind  (** Not a regular file or a directory *)
    | Error of Unix.error

  val equal : t -> t -> bool
end

(** Digest a path taking into account its [Stats_for_digest].

    - If the path is a regular file, the resulting digest includes the file's
      content as well as the its executable permissions bit.

    - If the path is a directory, the function attempts to do something sensible
      by digesting [Stats_for_digest] (except for the [st_kind] field since it's
      known to be [S_DIR] in this case).

    - Otherwise, the function returns [None]. *)
val path_with_stats : Path.t -> Stats_for_digest.t -> Path_digest_result.t

(** Like [path_with_stats] but raises on any non-[Ok] result. *)
val path_with_stats_exn : Path.t -> Stats_for_digest.t -> t

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
