open Stdune

type memory

type key

type metadata = Sexp.t list

exception Failed of string

val make : ?log:Log.t -> ?root:Path.t -> unit -> (memory, exn) Result.t

val default_root : unit -> Path.t

val key_to_string : key -> string

val key_of_string : string -> key

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
  -> (string * string) option
  -> promotion list

val search : memory -> key -> metadata * (Path.t * Path.t) list

val trim : memory -> int -> int * Path.t list
