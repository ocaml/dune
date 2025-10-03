open Import

type t

val of_string : string -> t
val of_string_opt : string -> t option
val of_string_user_error : Loc.t * string -> (t, User_message.t) result
val to_string : t -> string
val equal : t -> t -> bool
val compare : t -> t -> ordering
val hash : t -> int
val digest_feed : t Dune_digest.Feed.t
val to_dyn : t -> Dyn.t
val encode : t Encoder.t
val decode : t Decoder.t

include Comparable_intf.S with type key := t
