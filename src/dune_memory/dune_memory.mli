open Stdune

include module type of Dune_memory_intf

type 'a result = ('a, string) Result.t

val default_root : unit -> Path.t

val key_to_string : key -> string

val key_of_string : string -> key result

val promotion_to_string : promotion -> string

val command_to_dyn : command -> Dyn.t

module Memory : sig
  include memory

  val promote_sync :
       t
    -> (Path.Build.t * Digest.t) list
    -> key
    -> metadata
    -> int option
    -> (promotion list, string) Result.t

  val make : ?root:Path.t -> handler -> (t, string) Result.t
end

val trim : Memory.t -> int -> int * Path.t list

val make_caching : (module memory with type t = 'a) -> 'a -> (module caching)
