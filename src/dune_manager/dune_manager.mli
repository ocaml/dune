open Stdune

type t

exception Stop

exception Error of string

val make : ?root:Path.t -> ?log:Log.t -> unit -> t

val run : ?port_f:(string -> unit) -> ?port:int -> t -> unit

val stop : t -> unit

val endpoint : t -> string option
