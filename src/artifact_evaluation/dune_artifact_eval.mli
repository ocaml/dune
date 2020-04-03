type t

val encoded: string -> t
val get: t -> string option
val eval: string -> string option
