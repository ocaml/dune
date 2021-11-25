include Stdlib.Char

let is_digit = function
  | '0' .. '9' -> true
  | _non_digit_char -> false

let is_lowercase_hex = function
  | '0' .. '9'
  | 'a' .. 'f' ->
    true
  | _non_lowercase_hex_char -> false
