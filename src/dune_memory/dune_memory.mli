open Stdune

include module type of Dune_memory_intf

type 'a result = ('a, string) Result.t

val default_root : unit -> Path.t

val key_to_string : key -> string

val key_of_string : string -> key result

val promotion_to_string : promotion -> string

module Memory : sig
  include memory

  val promote_sync :
       t
    -> (Path.Build.t * Digest.t) list
    -> key
    -> metadata
    -> int option
    -> (promotion list, string) Result.t
end

val make : ?root:Path.t -> Memory.handler -> (Memory.t, string) Result.t

val trim : Memory.t -> int -> int * Path.t list
