(** Parsing and interpretation of opam files *)

open! Stdune

open OpamParserTypes

(** Type of opam files *)
type t = opamfile

(** Load a file *)
val load : Path.t -> t

(** Extracts a field *)
val get_field : t -> string -> value option

(** Parse the contents of an opam file *)
val parse : Lexing.lexbuf -> t

(** Replace all [pos] value by a triplet [(fname, line,
    absolute_offset)] *)
val absolutify_positions : file_contents:string -> opamfile -> opamfile
