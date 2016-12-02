type t = Cmi | Cmo | Cmx

val all : t list

val ext : t -> string
val compiler : t -> Context.t -> Path.t option
val source : t -> Ml_kind.t
