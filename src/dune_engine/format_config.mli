(** Represent the [(formatting)] field in [dune-project] files *)

open Import

module Language : sig
  (** Dune can format either source files through external programs (ocaml and
      reason are builtin dialects) or dune files *)
  type t =
    | Dialect of string
    | Dune
end

module Generic : sig
  type 'files t

  val field :
       since:Dune_lang.Syntax.Version.t
    -> default_files:'files
    -> files:('files, Dune_lang.Decoder.fields) Dune_lang.Decoder.parser
    -> 'files t option Dune_lang.Decoder.fields_parser

  val equal : files:('files -> 'files -> bool) -> 'files t -> 'files t -> bool

  val set_files : 'files1 t -> 'files2 -> 'files2 t

  val files : 'files t -> 'files
end

type t = unit Generic.t

val of_config :
  ext:t option -> dune_lang:t option -> version:Dune_lang.Syntax.Version.t -> t

(** The syntax corresponding to the dune 1.x [(using fmt)] extension. *)
val syntax : Dune_lang.Syntax.t

(** Where the configuration was defined. Can be [Loc.none] if formatting is done
    by default. *)
val loc : 'files Generic.t -> Loc.t

(** Should we emit formatting rules for a particular [language]? *)
val includes : 'files Generic.t -> Language.t -> bool

val is_empty : 'files Generic.t -> bool

(** Parse arguments for the 1.x extension. *)
val dparse_args : (t * Dune_lang.Stanza.Parser.t list) Dune_lang.Decoder.t

val to_dyn : t -> Dyn.t

(** Parse the contents of the dune2 [(formatting)] option.*)
val field :
  since:Dune_lang.Syntax.Version.t -> t option Dune_lang.Decoder.fields_parser

val encode_opt : t -> Dune_lang.t option

val equal : t -> t -> bool
