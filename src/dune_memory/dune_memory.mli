open Stdune

type key = Digest.t

type metadata = Sexp.t list

val default_root : unit -> Path.t

val key_to_string : key -> string

val key_of_string : string -> key

type promotion =
  | Already_promoted of Path.t * Path.t
  | Promoted of Path.t * Path.t

val promotion_to_string : promotion -> string

module type memory = sig
  type t

  val promote :
       t
    -> (Path.t * Digest.t) list
    -> key
    -> metadata
    -> (string * string) option
    -> (promotion list, string) Result.t

  val search : t -> key -> (metadata * (Path.t * Path.t) list, string) Result.t
end

module Memory : memory

val make : ?log:Log.t -> ?root:Path.t -> unit -> (Memory.t, string) Result.t

val trim : Memory.t -> int -> int * Path.t list
