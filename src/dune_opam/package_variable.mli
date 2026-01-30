open Import

module Scope : sig
  type t =
    | Self
    | Package of Package_name.t
end

type t =
  { name : Package_variable_name.t
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
