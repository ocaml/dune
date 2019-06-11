open Stdune

type memory

type key

type metadata = Sexp.t

exception Failed of string

val make : ?log:Log.t -> Path.t -> memory

type promotion =
  | Already_promoted of Path.t * Path.t
  | Promoted of Path.t * Path.t
  | Hash_mismatch of Path.t * Digest.t * Digest.t

val key : (Path.t * Digest.t) list -> metadata -> Path.t list -> key

val promotion_to_string : promotion -> string

val promote :
     memory
  -> (Path.t * Digest.t) list
  -> key
  -> metadata
  -> int option
  -> promotion list

val search : memory -> key -> metadata * (Path.t * Path.t) list
