open Stdune

include module type of Dune_cache_intf

type 'a result = ('a, string) Result.t

val default_root : unit -> Path.t

module Key : sig
  type t = Digest.t

  val of_string : string -> (t, string) Result.t

  val to_string : t -> string
end

val promotion_to_string : promotion -> string

val command_to_dyn : command -> Dyn.t

module Cache : sig
  include Cache

  val promote_sync :
       t
    -> (Path.Build.t * Digest.t) list
    -> Key.t
    -> metadata
    -> int option
    -> (promotion list, string) Result.t

  val make :
       ?root:Path.t
    -> ?duplication_mode:duplication_mode
    -> handler
    -> (t, string) Result.t
end

(** The size overhead of cached files. That is, the total size of cached files
    that are not linked in a build directory. *)
val size : Cache.t -> int

(** [trim cache size] removes files from [cache], starting with the least
    recently used one, until [size] bytes have been freed. *)
val trim : Cache.t -> int -> trimming_result

(** Purge invalid or incomplete cached rules. *)
val garbage_collect : Cache.t -> trimming_result

val make_caching : (module Cache with type t = 'a) -> 'a -> (module Caching)
