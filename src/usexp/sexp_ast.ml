module Loc = struct
  type t =
    { start : Lexing.position
    ; stop  : Lexing.position
    }
end

type atom = A of string [@@unboxed]

type t =
  | Atom of Loc.t * atom
  | Quoted_string of Loc.t * string
  | List of Loc.t * t list
