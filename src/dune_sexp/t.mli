(** Parsing of s-expressions.

    This library is internal to dune and guarantees no API stability.*)
open! Stdune

(** The S-expression type *)
type t =
  | Atom of Atom.t
  | Quoted_string of string
  | List of t list
  | Template of Template.t

(** [atom s] convert the string [s] to an Atom. NOTE No validity check is
    performed. *)
val atom : string -> t

val atom_or_quoted_string : string -> t

(** Serialize a S-expression *)
val to_string : t -> string

(** Serialize a S-expression using indentation to improve readability *)
val pp : t -> _ Pp.t

val to_dyn : t Dyn.builder
val to_sexp : t -> Sexp.t
