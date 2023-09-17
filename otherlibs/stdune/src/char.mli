include module type of struct
  include Stdlib.Char
end

(** Check if a character belongs to the set [{'0'..'9'}]. *)
val is_digit : t -> bool

(** Check if a character belongs to the set [{'0'..'9', 'a'..'f'}]. *)
val is_lowercase_hex : t -> bool

val hash : t -> int
val compare : t -> t -> Ordering.t
