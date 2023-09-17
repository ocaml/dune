type t =
  { name : string option
  ; entries : entry list
  }

and entry =
  | Comment of string
  | Rule of rule
  | Package of t

and rule =
  { var : string
  ; predicates : predicate list
  ; action : action
  ; value : string
  }

and action =
  | Set
  | Add

and predicate =
  | Pos of string
  | Neg of string

module Parse : sig
  val entries : Lexing.lexbuf -> int -> entry list -> entry list
end
