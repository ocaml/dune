module Atom = struct
  type t = A of string [@@unboxed]
end

type t =
  | Atom of Atom.t
  | Quoted_string of string
  | List of t list

module Loc = struct
  type t =
    { start : Lexing.position
    ; stop  : Lexing.position
    }
end

module Template = struct
  type sexp = t

  type syntax = Dollar_brace | Dollar_paren | Percent

  type var =
    { loc: Loc.t
    ; name: string
    ; payload: string
    ; syntax: syntax
    }

  type part =
    | Text of string
    | Var of var

  type t =
    { quoted: bool
    ; parts: part list
    ; loc: Loc.t
    }
end
