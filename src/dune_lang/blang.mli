open Stdune
open Dune_sexp

module Op : sig
  type t =
    | Eq
    | Gt
    | Gte
    | Lte
    | Lt
    | Neq

  val eval : t -> Ordering.t -> bool
end

(* Note that this type is defined separately from [_ Ast.t] so that its
   constructors are scoped within the [Blang] module, allowing us to construct
   and pattern-match on them without needing to refer to [Blang.Ast]. *)
type 'string ast =
  | Const of bool
  | Not of 'string ast
  | Expr of 'string
  | And of 'string ast list
  | Or of 'string ast list
  | Compare of Op.t * 'string * 'string

type t = String_with_vars.t ast

val true_ : t
val false_ : t
val to_dyn : t -> Dyn.t
val decode : t Decoder.t
val encode : t Encoder.t

module Ast : sig
  type 'string t = 'string ast

  val true_ : 'string t
  val false_ : 'string t
  val to_dyn : 'string Dyn.builder -> 'string t -> Dyn.t
  val decode : 'string Decoder.t -> 'string t Decoder.t
  val encode : 'string Encoder.t -> 'string t Encoder.t
end
