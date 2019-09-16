open Stdune

type key = Digest.t

type metadata = Sexp.t list

type 'a result = ('a, string) Result.t

val default_root : unit -> Path.t

val key_to_string : key -> string

val key_of_string : string -> key result

type promotion =
  | Already_promoted of Path.t * Path.t
  | Promoted of Path.t * Path.t

val promotion_to_string : promotion -> string

module File : sig
  type t =
    { in_the_memory : Path.t
    ; in_the_build_directory : Path.t
    ; digest : Digest.t
    }
end

module type memory = sig
  type t

  val promote :
       t
    -> (Path.t * Digest.t) list
    -> key
    -> metadata
    -> (string * string) option
    -> (promotion list, string) Result.t

  val search :
    t -> ?touch:bool -> key -> (metadata * File.t list, string) Result.t

  val set_build_dir : t -> Path.t -> t
end

module Memory : memory

val make : ?root:Path.t -> unit -> (Memory.t, string) Result.t

val trim : Memory.t -> int -> int * Path.t list
