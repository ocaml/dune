module Loc = struct
  type t =
    { start : Lexing.position
    ; stop  : Lexing.position
    }
end

type t =
  | Atom of Loc.t * string
  | List of Loc.t * t list
