open Import

module Name : sig
  type t

  val to_opam : t -> OpamVariable.t
  val of_opam : OpamVariable.t -> t
  val compare : t -> t -> Ordering.t
  val to_dyn : t -> Dyn.t
  val encode : t -> Dune_sexp.t

  module Map : Map.S with type key = t

  val of_string : string -> t
  val to_string : t -> string
end

module Scope : sig
  type t =
    | Self
    | Package of Package_name.t
end

type t =
  { name : Name.t
  ; scope : Scope.t
  }

val compare : t -> t -> Ordering.t

include Comparable_intf.S with type key := t

val to_dyn : t -> Dyn.t

(** [of_macro_invocation ~loc macro_invocation] interprets a macro invocation
    as a package variable. It's assumed that the macro invocation was created
    using the [pform_of_opam_ident] function. This function expects the macro
    to be [Pkg] or [Pkg_self] or an error is returned. *)
val of_macro_invocation
  :  loc:Loc.t
  -> Pform.Macro_invocation.t
  -> (t, [ `Unexpected_macro ]) result

val to_pform : t -> Pform.t
