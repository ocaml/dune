type t = Chrome_trace.Json.t

val to_string : t -> string
val string : string -> t
val assoc : (string * t) list -> t
val list : t list -> t
val int : int -> t
val float : float -> t
