(** Digest files with [mtime]-based caching persisted between builds. *)
open Stdune

module Digest_result : sig
  type t =
    | Ok of Digest.t
    | No_such_file
    | Broken_symlink
    | Unexpected_kind of File_kind.t
    | Unix_error of Unix_error.Detailed.t  (** Can't be [ENOENT]. *)
    | Error of exn

  val to_option : t -> Digest.t option
end

(** Digest the contents of a build artifact. *)
val build_file : Path.Build.t -> Digest_result.t

module Untracked : sig
  (** Digest the contents of a source or external file. This function doesn't
      track the source file. For a tracked version, see [fs_memo.mli]. *)
  val source_or_external_file : Path.t -> Digest_result.t
end

(** Same as [build_file], but forces the digest of the file to be re-computed.

    If [remove_write_permissions] is true, also remove write permissions on the
    file. *)
val refresh : Path.Build.t -> remove_write_permissions:bool -> Digest_result.t

(** {1 Managing the cache} *)

(** Update the digest for a file in the cache. Records the current [mtime]. *)
val set : Path.Build.t -> Digest.t -> unit

(** Remove a file from the digest cache. *)
val remove : Path.Build.t -> unit

(** Invalidate all cached [mtime] values. This causes all subsequent calls to
    [build_file] or [source_or_external_file] to incur an additional [stat] call
    to read the current [mtime]. *)
val invalidate_cached_timestamps : unit -> unit
