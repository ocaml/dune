open Import

type t =
  { name : Package_name.t
  ; constraint_ : Package_constraint.t option
  }

val encode : t Encoder.t
val decode : (Loc.t * t) Decoder.t
val to_dyn : t -> Dyn.t
val equal : t -> t -> bool
