type t

exception Parse_error

val create : string -> t
val junk : t -> unit
val unget : t -> unit
val eos : t -> bool
val test : t -> char -> bool
val test2 : t -> char -> char -> bool
val get : t -> char
val accept : t -> char -> bool
val accept2 : t -> char -> char -> bool
val accept_s : t -> string -> bool
val integer : t -> int option
