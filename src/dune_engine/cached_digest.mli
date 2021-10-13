(** Digest files with [mtime]-based caching persisted between builds. *)
open Stdune

(** Digest the contents of a build artifact. *)
val build_file : Path.Build.t -> Digest.t

module Untracked : sig
  (** Digest the contents of a source or external file. This function doesn't
      track the source file. For a tracked version, see [fs_memo.mli]. *)
  val source_or_external_file : Path.t -> Digest.t
end

module Refresh_result : sig
  type t =
    | Ok of Digest.t
    | No_such_file
    | Error of exn  (** Can't be [Unix.ENOENT]. *)
end

(** Same as [build_file], but forces the digest of the file to be re-computed.

    If [remove_write_permissions] is true, also remove write permissions on the
    file. *)
val refresh : Path.Build.t -> remove_write_permissions:bool -> Refresh_result.t

(** {1 Managing the cache} *)

(** Update the digest for a file in the cache. Records the current [mtime]. *)
val set : Path.Build.t -> Digest.t -> unit

(** Remove a file from the digest cache. *)
val remove : Path.Build.t -> unit

(** Invalidate all cached [mtime] values. This causes all subsequent calls to
    [build_file] or [source_or_external_file] to incur an additional [stat] call
    to read the current [mtime]. *)
val invalidate_cached_timestamps : unit -> unit
