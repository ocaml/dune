(** Parsing and interpretation of opam files *)

open OpamParserTypes

(** Type of opam files *)
type t = opamfile

(** Load a file *)
val load : string -> t

(** Extracts a field *)
val get_field : t -> string -> value option
