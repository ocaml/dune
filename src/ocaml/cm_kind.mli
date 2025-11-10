open Stdune

type t =
  | Cmi
  | Cmo
  | Cmx
  | Cmt
  | Cmti

val compare : t -> t -> Ordering.t
val all : t list
val ext : t -> string
val source : t -> Ml_kind.t
val to_dyn : t -> Dyn.t
