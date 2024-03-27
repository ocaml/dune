(** Digest files with [stat]-based caching persisted between builds. *)

open Import

module Digest_result : sig
  module Error : sig
    type t =
      | No_such_file
      | Broken_symlink
      | Cyclic_symlink
      | Unexpected_kind of File_kind.t
      | Unix_error of Unix_error.Detailed.t (** Can't be [ENOENT]. *)
      | Unrecognized of exn

    val to_dyn : t -> Dyn.t
  end

  type t = (Digest.t, Error.t) result

  val equal : t -> t -> bool
  val to_option : t -> Digest.t option
  val to_dyn : t -> Dyn.t
end

(** Digest the contents of a build artifact. *)
val build_file : Path.Build.t -> Digest_result.t

(** Same as [build_file], but forces the digest of the file to be re-computed.

    If [remove_write_permissions] is true, also remove write permissions on the
    file. *)
val refresh : remove_write_permissions:bool -> Path.Build.t -> Digest_result.t

module Untracked : sig
  (** Digest the contents of a source or external file. This function doesn't
      track the source file. For a tracked version, see [fs_memo.mli]. *)
  val source_or_external_file : Path.Outside_build_dir.t -> Digest_result.t

  (** Invalidate the cached [stat] value. This causes the subsequent call to
      [source_or_external_file] to incur an additional [stat] call. *)
  val invalidate_cached_timestamp : Path.Outside_build_dir.t -> unit
end

(** {1 Managing the cache} *)

(** Update the digest for a file in the cache. Records the current [stat]. *)
val set : Path.Build.t -> Digest.t -> unit

(** Remove a file from the digest cache. *)
val remove : Path.Build.t -> unit

(** Invalidate all cached [stat] values. This causes all subsequent calls to
    [build_file] or [source_or_external_file] to incur additional [stat] calls. *)
val invalidate_cached_timestamps : unit -> unit

(** The reduced set of file stats this module inspects to decide whether a file
    changed or not *)
module Reduced_stats : sig
  type t

  val to_dyn : t -> Dyn.t
  val of_unix_stats : Unix.stats -> t
  val compare : t -> t -> Ordering.t
end
