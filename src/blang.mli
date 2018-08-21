open! Stdune

module Op : sig
  type t =
    | Eq
    | Gt
    | Gte
    | Lte
    | Lt
    | Neq
end

type 'a t =
  | Expr of 'a
  | And of 'a t list
  | Or of 'a t list
  | Compare of Op.t * 'a * 'a

type 'a expander =
  { f : 'value. mode:'value String_with_vars.Mode.t
      -> 'a
      -> Loc.t * 'value
  }

val eval_bool : 'a t -> dir:Path.t -> f:'a expander -> bool
