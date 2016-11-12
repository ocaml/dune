(** META file representation *)

type t =
  { name : string
  ; sub  : t list
  ; defs : def list
  }

and kind = Set | Add

and def =
  { kind       : kind
  ; var        : string
  ; predicates : predicate list
  }

and predicate =
  | Pos of string
  | Neg of string

