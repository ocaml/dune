(** Digest files with caching *)
open Stdune

(** Digest the contents of the following source or external file *)
val source_or_external_file : Path.t -> Digest.t Memo.Build.t

(* jeremiedimino: is it safe to export [peek_file]? *)

(** The digest of the following file, if cached *)
val peek_file : Path.t -> Digest.t option

(** Digest the contents of an artefact *)
val build_file : Path.Build.t -> Digest.t

(** Same as [build_file], but forces the digest of the file to be re-computed *)
val refresh : Path.Build.t -> Digest.t

(** Same as {!refresh} but also remove write permissions on the file *)
val refresh_and_chmod : Path.Build.t -> Digest.t

(** {1 Managing the cache} *)

(** Update the digest for a file in the cache *)
val set : Path.t -> Digest.t -> unit

(** Clear the following digest from the cache *)
val remove : Path.t -> unit

(** Invalidate cached timestamp *)
val invalidate_cached_timestamps : unit -> unit
