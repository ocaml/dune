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

val of_macro_invocation : Pform.Macro_invocation.t -> t option

(** Parse an opam variable name. Identifiers begining with "<package>:" are
    treated as package-scoped variables unless <package> is "_" in which case
    they are treated as self-scoped. Identifiers without the "<package>:"
    prefix are treated as self-scoped unless they are the name of an opam
    global variable. Global variables are encoded as pform variables while all
    other variables are encoded as pform macros with the [Macro.Pkg] macro. *)
val pform_of_opam_ident : string -> Pform.t
