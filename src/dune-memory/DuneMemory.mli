open Stdune

type memory

exception Failed of string

val make : ?log:Log.t -> Path.t -> memory

type promotion =
  | Already_promoted of Path.t * Path.t
  | Promoted of Path.t * Path.t
  | Hash_mismatch of Path.t * Digest.t * Digest.t

val promotion_to_string : promotion -> string

val promote :
  memory -> (Path.t * Digest.t) list -> Sexp.t -> int option -> promotion list

val search : memory -> Sexp.t -> (Path.t * Path.t) list
