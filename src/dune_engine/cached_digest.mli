(** Digest files with caching *)
open Stdune

(** Digest the contents of the following source or external file *)
val source_or_external_file : Path.t -> Digest.t Memo.Build.t

(* jeremiedimino: is it safe to export [peek_file]? *)

(** The digest of the following file, if cached *)
val peek_file : Path.t -> Digest.t option

(** Digest the contents of an artefact *)
val build_file : Path.Build.t -> Digest.t

module Refresh_result : sig
  type t =
    | Ok of Digest.t
    | No_such_file
    | Error of exn
end

(** Same as [build_file], but forces the digest of the file to be re-computed.

    If [remove_write_permissions] is true, also remove write permissions on the file. *)
val refresh : Path.Build.t -> remove_write_permissions:bool -> Refresh_result.t

(** {1 Managing the cache} *)

(** Update the digest for a file in the cache *)
val set : Path.t -> Digest.t -> unit

(** Clear the following digest from the cache *)
val remove : Path.t -> unit

(** Invalidate cached timestamp *)
val invalidate_cached_timestamps : unit -> unit
