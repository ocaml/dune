module Char = Caml.Char

include struct
  [@@@warning "-32-3"]
  let uppercase_ascii    = Char.uppercase
  let lowercase_ascii    = Char.lowercase
end

include Char
