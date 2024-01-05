type t = OpamUrl.t

val equal : t -> t -> bool
val hash : t -> int
val to_string : t -> string
val of_string : string -> t
val decode_loc : (Stdune.Loc.t * t) Dune_sexp.Decoder.t
val rev : t -> string option
val base_url : t -> string
val is_version_control : t -> bool
