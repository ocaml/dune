open Stdune
module Utils = Utils

type t

exception Error of string

type config = { exit_no_client : bool }

val make : ?root:Path.t -> config:config -> unit -> t

val default_port_file : unit -> Path.t

val check_port_file :
     ?close:bool
  -> Path.t
  -> ((string * int * Unix.file_descr) option, exn) Result.t

val run : ?port_f:(string -> unit) -> ?port:int -> t -> unit

val stop : t -> unit

val endpoint : t -> string option

val daemon : root:Path.t -> config:config -> (string -> unit) -> unit

module Client : sig
  type t

  val promote :
       t
    -> (Path.t * Digest.t) list
    -> Dune_memory.key
    -> Dune_memory.metadata
    -> int option
    -> (unit, string) Result.t

  val search :
       t
    -> Dune_memory.key
    -> ( Dune_memory.metadata * (Path.t * Path.t * Digest.t) list
      , string )
       Result.t

  val set_build_dir : t -> Path.t -> unit

  val make : unit -> (t, exn) Result.t

  val teardown : t -> unit
end
