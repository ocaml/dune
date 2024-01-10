open! Stdune

type t =
  { name : Package_name.t
  ; constraint_ : Package_constraint.t option
  }

val encode : t Dune_sexp.Encoder.t
val decode : t Dune_sexp.Decoder.t
val to_dyn : t -> Dyn.t
val equal : t -> t -> bool

module Opam_compatible : sig
  type package_dependency = t

  type t =
    { name : Package_name.Opam_compatible.t
    ; constraint_ : Package_constraint.t option
    }

  val encode : t Dune_sexp.Encoder.t
  val decode : t Dune_sexp.Decoder.t
  val to_dyn : t -> Dyn.t
  val equal : t -> t -> bool
  val of_package_dependency : package_dependency -> t
end
