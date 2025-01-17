type t

val source : t -> Source_kind.t option
val license : t -> string list option
val authors : t -> string list option
val homepage : t -> string option
val bug_reports : t -> string option
val documentation : t -> string option
val maintainers : t -> string list option
val maintenance_intent : t -> string list option

(** example package info (used for project initialization ) *)
val example
  :  authors:string list option
  -> maintainers:string list option
  -> license:string list option
  -> t

val empty : t
val to_dyn : t Dyn.builder
val encode_fields : t -> Dune_sexp.t list

val decode
  :  ?since:Dune_sexp.Syntax.Version.t
  -> unit
  -> t Dune_sexp.Decoder.fields_parser

val decode_maintenance_intent : string list Dune_sexp.Decoder.t
val superpose : t -> t -> t

val create
  :  maintainers:string list option
  -> maintenance_intent:string list option
  -> authors:string list option
  -> homepage:string option
  -> bug_reports:string option
  -> documentation:string option
  -> license:string list option
  -> source:Source_kind.t option
  -> t
