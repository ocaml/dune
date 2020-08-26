(** Digest files with caching *)
open Stdune

(** Digest the contents of the following file *)
val file : Path.t -> Digest.t

(** The digest of the following file, if cached *)
val peek_file : Path.t -> Digest.t option

(** Clear the following digest from the cache *)
val remove : Path.t -> unit

(** Same as {!file} but forces the digest to be recomputed *)
val refresh : Path.t -> Digest.t

(** Same as {!refresh} remove write permissions on the file *)
val refresh_and_chmod : Path.t -> Digest.t

(** Update the digest for a file in the cache *)
val set : Path.t -> Digest.t -> unit

(** Invalidate cached timestamp *)
val invalidate_cached_timestamps : unit -> unit
