(** Represent the [(formatting)] field in [dune-project] files *)

open Stdune

module Language : sig
  (** Dune can format either source files through external programs (ocaml and
      reason are builtin dialects) or dune files *)
  type t =
    | Dialect of string
    | Dune
end

type t

val of_config
  :  ext:t option
  -> dune_lang:t option
  -> version:Dune_sexp.Syntax.Version.t
  -> t

(** The syntax corresponding to the dune 1.x [(using fmt)] extension. *)
val syntax : Dune_sexp.Syntax.t

(** Where the configuration was defined. Can be [Loc.none] if formatting is done
    by default. *)
val loc : t -> Loc.t

(** Should we emit formatting rules for a particular [language]? *)
val includes : t -> Language.t -> bool

val is_empty : t -> bool

(** Parse arguments for the 1.x extension. *)
val dparse_args : (t * Stanza.Parser.t list) Dune_sexp.Decoder.t

val to_dyn : t -> Dyn.t

(** Parse the contents of the dune2 [(formatting)] option.*)
val field : since:Dune_sexp.Syntax.Version.t -> t option Dune_sexp.Decoder.fields_parser

val encode_opt : t -> Dune_sexp.t option
val equal : t -> t -> bool
