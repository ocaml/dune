open! Stdune
open Dune_lang

module Name : sig
  type t

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

(** [of_macro_invocation ~loc macro_invocation] interprets a macro invocation
    as a package variable. It's assumed that the macro invocation was created
    using the [pform_of_opam_ident] function. This function expects the macro
    to be [Pkg] or [Pkg_self] or an error is returned. *)
val of_macro_invocation
  :  loc:Loc.t
  -> Pform.Macro_invocation.t
  -> (t, [ `Unexpected_macro ]) result

val to_pform : t -> Pform.t

(** Parse an opam variable name.

    Identifiers beginning with "<package>:" are treated as package-scoped variables unless
    <package> is "_" in which case they are treated as self-scoped.

    Identifiers without the "<package>:" prefix are treated as self-scoped unless they are
    the name of an opam global variable.

    Global variables are encoded as pform variables while all other variables are encoded
    as pform macros with the [Macro.Pkg] macro.

    Returns an [Error] if the identifier is not a valid opam variable name. *)
val pform_of_opam_ident : string -> (Pform.t, [ `Root_unsupported ]) result
