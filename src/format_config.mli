(** Represent the [(formatting)] field in [dune-project] files *)

open! Stdune
open Import

module Language : sig
  (** Dune can format either source files through external programs (ocaml and
      reason are builtin dialects) or dune files *)
  type t = Dialect of string | Dune
end

type t

val of_config
  :  ext:t option
  -> dune_lang:t option
  -> t option

val syntax : Syntax.t
(** The syntax corresponding to the dune 1.x [(using fmt)] extension. *)

val loc : t -> Loc.t
(** Where the configuration was defined. Can be [Loc.none] if formatting is done
    by default. *)

val includes : t -> Language.t -> bool
(** Should we emit formatting rules for a particular [language]? *)

val dparse_args : (t * Stanza.Parser.t list) Dune_lang.Decoder.t
(** Parse arguments for the 1.x extension. *)

val to_dyn : t -> Dyn.t

val field : t option Dune_lang.Decoder.fields_parser
(** Parse the contents of the dune2 [(formatting)] option.*)
