open Import

type t = OpamUrl.t

val to_dyn : t -> Dyn.t
val compare : t -> t -> Ordering.t
val decode_loc : (Loc.t * t) Decoder.t
val to_string : t -> string
