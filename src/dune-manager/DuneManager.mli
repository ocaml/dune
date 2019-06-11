open Stdune

type t

exception Stop

exception Error of string

val make : Path.t -> t

val run : ?port_f:(int -> unit) -> ?port:int -> t -> unit

val stop : t -> unit
