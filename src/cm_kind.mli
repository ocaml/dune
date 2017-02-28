type t = Cmi | Cmo | Cmx

val all : t list

val ext : t -> string
val source : t -> Ml_kind.t
