module Utf8 : sig
  val next_utf8_length : string -> int -> int
  val is_utf8_valid : string -> int -> int -> bool
end

val escaped : string -> string
val quoted : string -> string
