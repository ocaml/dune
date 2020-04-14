(** Digest files with caching *)
open Stdune

val file : Path.t -> Digest.t
(** Digest the contents of the following file *)

val peek_file : Path.t -> Digest.t option
(** The digest of the following file, if cached *)

val remove : Path.t -> unit
(** Clear the following digest from the cache *)

val refresh : Path.t -> Digest.t
(** Same as {!file} but forces the digest to be recomputed *)

val refresh_and_chmod : Path.t -> Digest.t
(** Same as {!refresh} remove write permissions on the file *)

val set : Path.t -> Digest.t -> unit
(** Update the digest for a file in the cache *)

val invalidate_cached_timestamps : unit -> unit
(** Invalidate cached timestamp *)
