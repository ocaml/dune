open Stdune
module Utils = Utils

type t

exception Stop

exception Error of string

val make : ?root:Path.t -> ?log:Log.t -> unit -> t

val default_port_file : unit -> Path.t

val check_port_file :
     ?close:bool
  -> Path.t
  -> ((string * int * Unix.file_descr) option, exn) Result.t

val run : ?port_f:(string -> unit) -> ?port:int -> t -> unit

val stop : t -> unit

val endpoint : t -> string option

module Client : sig
  include Dune_memory.memory

  val make : ?log:Stdune.Log.t -> unit -> (t, exn) Result.t
end
