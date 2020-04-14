(** Parsing of s-expressions.

    This library is internal to dune and guarantees no API stability.*)
open! Stdune

(** The S-expression type *)
type t =
  | Atom of Atom.t
  | Quoted_string of string
  | List of t list
  | Template of Template.t

val atom : string -> t
(** [atom s] convert the string [s] to an Atom. NOTE No validity check is
    performed. *)

val atom_or_quoted_string : string -> t

val unsafe_atom_of_string : string -> t

val to_string : t -> string
(** Serialize a S-expression *)

val pp : t -> _ Pp.t
(** Serialize a S-expression using indentation to improve readability *)

module Deprecated : sig
  val pp : Format.formatter -> t -> unit
  (** Serialize a S-expression using indentation to improve readability *)

  val pp_split_strings : Format.formatter -> t -> unit
  (** Same as [pp ~syntax:Dune], but split long strings. The formatter must have
      been prepared with [prepare_formatter]. *)

  val prepare_formatter : Format.formatter -> unit
  (** Prepare a formatter for [pp_split_strings]. Additionaly the formatter
      escape newlines when the tags "makefile-action" or "makefile-stuff" are
      active. *)
end

val to_dyn : t Dyn.Encoder.t
