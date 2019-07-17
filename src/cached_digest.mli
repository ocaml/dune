(** Digest files with caching *)
open Stdune

(** Digest the contents of the following file *)
val file : Path.t -> Digest.t

(** Clear the following digest from the cache *)
val remove : Path.t -> unit

(** Same as {!file} but forces the digest to be recomputed *)
val refresh : Path.t -> Digest.t

(** Invalidate cached timestamp *)
val invalidate_cached_timestamps : unit -> unit
