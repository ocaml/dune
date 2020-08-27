open! Dune_engine
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

type t =
  | Const of bool
  | Expr of String_with_vars.t
  | And of t list
  | Or of t list
  | Compare of Op.t * String_with_vars.t * String_with_vars.t

val true_ : t

val fold_vars : t -> init:'a -> f:(String_with_vars.Var.t -> 'a -> 'a) -> 'a

val eval :
  t -> dir:Path.t -> f:Value.t list option String_with_vars.expander -> bool

val to_dyn : t -> Dyn.t

val decode : t Dune_lang.Decoder.t
