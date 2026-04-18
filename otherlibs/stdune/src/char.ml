include Stdlib.Char

let repr = Repr.char

let is_digit = function
  | '0' .. '9' -> true
  | _non_digit_char -> false
;;

let is_lowercase_hex = function
  | '0' .. '9' | 'a' .. 'f' -> true
  | _non_lowercase_hex_char -> false
;;

let[@inline always] hash c = Int.hash (code c)
let compare x y = Ordering.of_int (compare x y)
let to_dyn = Repr.to_dyn repr
