module Template = struct
  type var_syntax = Dollar_brace | Dollar_paren | Percent

  type var =
    { loc: Loc.t
    ; name: string
    ; payload: string option
    ; syntax: var_syntax
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

module Sexp = struct
  type t =
    | Atom of Atom.t
    | Quoted_string of string
    | List of t list
    | Template of Template.t
end
