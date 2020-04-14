(** Parsing and interpretation of opam files *)

open! Stdune
open OpamParserTypes

(** Type of opam files *)
type t = opamfile

val load : Path.t -> t
(** Load a file *)

val get_field : t -> string -> value option
(** Extracts a field *)

val parse : Lexing.lexbuf -> t
(** Parse the contents of an opam file *)

val parse_value : Lexing.lexbuf -> value
(** Parse just a value *)

val absolutify_positions : file_contents:string -> opamfile -> opamfile
(** Replace all [pos] value by a triplet [(fname, line, absolute_offset)] *)

val nopos : OpamParserTypes.pos

val existing_variables : t -> String.Set.t

module Create : sig
  open OpamParserTypes

  val string : string -> value

  val list : ('a -> value) -> 'a list -> value

  val string_list : string list -> value

  val normalise_field_order : (string * value) list -> (string * value) list

  val of_bindings : (string * value) list -> file:Path.t -> t
end
