type t

val of_string : string -> t

val substitute_all : t -> f:(string -> string) -> string
