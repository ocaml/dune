open Stdune

type t

exception Stop

exception Error of string

val make : ?root:Path.t -> unit -> t

val run : ?port_f:(int -> unit) -> ?port:int -> t -> unit

val stop : t -> unit
