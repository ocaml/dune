open! Stdune

module Op : sig
  type t =
    | Eq
    | Gte
    | Lte
    | Gt
    | Lt
    | Neq
end

module Var : sig
  type t =
    | Literal of string
        (** A quoted string literal, such as a version number *)
    | Var of string
        (** A variable name such as :version or :with-test (the leading ":" is
            excluded from the string) *)
end

type t =
  | Bvar of Var.t
  | Uop of Op.t * Var.t
  | Bop of Op.t * Var.t * Var.t
  | And of t list
  | Or of t list

val encode : t Dune_sexp.Encoder.t

val decode : t Dune_sexp.Decoder.t

val to_dyn : t -> Dyn.t
