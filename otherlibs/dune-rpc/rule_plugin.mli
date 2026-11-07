(** Shared s-expressions and RPC declarations for rule plugins. *)

module Sexp : sig
  type pform =
    { name : string
    ; payload : string option
    }

  type template_part =
    | Text of string
    | Pform of pform

  type template =
    { quoted : bool
    ; parts : template_part list
    }

  type t =
    | Atom of string
    | Quoted_string of string
    | Template of template
    | List of t list
end

val sexp : Sexp.t Stdune.Conv.value
val load_rules : (Sexp.t list, Sexp.t list) Types.Decl.Request.t
val ready : (unit, unit) Types.Decl.Request.t
