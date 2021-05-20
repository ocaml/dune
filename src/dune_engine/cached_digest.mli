(** Digest files with caching *)
open Stdune

(** Digest the contents of a source or external file. *)
val source_or_external_file : Path.t -> Digest.t

(** Digest the contents of a build artifact. *)
val build_file : Path.Build.t -> Digest.t

module Refresh_result : sig
  type t =
    | Ok of Digest.t
    | No_such_file
    | Error of exn
end

(** Same as [build_file], but forces the digest of the file to be re-computed.

    If [remove_write_permissions] is true, also remove write permissions on the
    file. *)
val refresh : Path.Build.t -> remove_write_permissions:bool -> Refresh_result.t

(** {1 Managing the cache} *)

(** Update the digest for a file in the cache *)
val set : Path.t -> Digest.t -> unit

(** Clear the following digest from the cache *)
val remove : Path.t -> unit

(** Invalidate cached timestamp *)
val invalidate_cached_timestamps : unit -> unit
