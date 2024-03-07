open Dune_sexp

(* Note that this type is defined separately from [_ Ast.t] so that its
   constructors are scoped within the [Blang] module, allowing us to construct
   and pattern-match on them without needing to refer to [Blang.Ast]. *)
type 'string ast =
  | Const of bool
  | Not of 'string ast
  | Expr of 'string
  | And of 'string ast list
  | Or of 'string ast list
  | Compare of Relop.t * 'string * 'string

type t = String_with_vars.t ast

val true_ : t
val false_ : t
val to_dyn : t -> Dyn.t
val decode : t Decoder.t
val encode : t Encoder.t
val equal : t -> t -> bool

module Ast : sig
  type 'string t = 'string ast

  val true_ : 'string t
  val false_ : 'string t
  val to_dyn : 'string Dyn.builder -> 'string t -> Dyn.t

  (** The [override_decode_bare_literal] argument is an alternative parser that
      if provided, will be used to parse string literals for the [Expr _]
      constructor. This is intended to prevent infinite recursion when parsing
      blangs whose ['string] type is another DSL which is mutually recursive
      with blang (e.g. slang). *)
  val decode
    :  override_decode_bare_literal:'string Decoder.t option
    -> 'string Decoder.t
    -> 'string t Decoder.t

  val encode : 'string Encoder.t -> 'string t Encoder.t
end
