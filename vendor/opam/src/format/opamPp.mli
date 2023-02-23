(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2018 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Generic bidirectional transformation toolbox for parsing/printing *)

open OpamParserTypes.FullPos

(** {2 Parsing positions and error reporting helpers} *)

(** Format error reporting: position and message *)
type bad_format = pos option * string

(** All the following parsing function raise [Bad_format] in case the
    input does not have the right format. *)
exception Bad_format of bad_format
exception Bad_format_list of bad_format list
exception Bad_version of bad_format

(** Raise [Bad_format]. *)
val bad_format: ?pos:pos -> ('a, unit, string, 'b) format4 -> 'a

(** Raise [Bad_version]. *)
val bad_version: ?pos:pos -> ('a, unit, string, 'b) format4 -> 'a

val string_of_bad_format: ?file:string -> exn -> string

(** Adds a position to a Bad_format exception if it doesn't have one yet *)
val add_pos: pos -> exn -> exn

(** {2 Parser/printers} *)

(** The type of bidirectional parsers from ['a] to ['b]. We abuse the terms
    and describe going from ['a] to ['b] as "parsing", and going from ['b] to
    ['a] as "printing". Parsing is generally error-prone, while printing is
    not expected to fail, so the handling isn't really symmetrical.

    [parse (print x)] should always be the identity, while no guarantee is
    given regarding [print (parse x)] *)
type ('a, 'b) t = private {
  parse: pos:pos -> 'a -> 'b;
  print: 'b -> 'a;
  ppname: string;
  name_constr: string -> string;
}

(** Base constructor for Pp.t, from a parser function and a printer function.
    [name_constr] is used to construct the resulting name when on the left of
    a pipe. Names are for tracing errors. *)
val pp :
  ?name:string -> ?name_constr:(string -> string) ->
  (pos:pos -> 'a -> 'b) ->
  ('b -> 'a) ->
  ('a, 'b) t

(** Constructor of Pp.t from a name and a pair *)
val of_pair :
  string ->
  ('a -> 'b) * ('b -> 'a) ->
  ('a, 'b) t

(** Base call for parsing with a pp *)
val parse : ('a, 'b) t -> pos:pos -> 'a -> 'b

(** Base call for printing with a pp *)
val print : ('a, 'b) t -> 'b -> 'a

(** Error handling *)

(** Raises an exception handled by parser calls *)
val unexpected : ?pos:pos -> unit -> 'a

(** {3 Various pp constructors} *)

module Op : sig

  (** Piping pps together: the left-hand pp is called first when parsing, last
      when printing *)
  val ( -| ) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t

  (** Combinator to parse lists to different types using nested pairs *)
  val ( ^+ ) : ('a, 'b) t -> ('a list, 'c) t -> ('a list, 'b * 'c) t

end

val identity : ('a, 'a) t

(** Always parses to [None] *)
val ignore : ('a, 'b option) t

(** Identity pp, unless the check fails. The check is turned into an assertion
    when printing. If no [errmsg] is given, raises [Unexpected], otherwise
    call [raise] with the given [errmsg]. By default [raise] raises
    [Bad_format]. *)
val check :
  ?name:string ->
  ?raise:(?pos:pos -> (string -> 'a, unit, string, 'a) format4
          -> string -> 'a) ->
  ?errmsg:string -> ('a -> bool) -> ('a, 'a) t

val map_pair :
  ?name:string ->
  ?posf1:('a -> pos) ->
  ?posf2:('b -> pos) ->
  ('a, 'c) t -> ('b, 'd) t -> ('a * 'b, 'c * 'd) t

(** Builds a pp of pairs by passing the second term along *)
val map_fst :
  ('a, 'b) t -> ('a * 'c, 'b * 'c) t

(** Builds a pp of pairs by passing the first term along *)
val map_snd :
  ('a, 'b) t -> ('c * 'a, 'c * 'b) t

val map_list :
  ?name:string ->
  ?posf:('a -> pos) -> ('a, 'b) t -> ('a list, 'b list) t

val map_option : ?name:string -> ('a, 'b) t -> ('a option, 'b option) t

(** Parsing fails on non-singleton lists *)
val singleton : ('a list, 'a) t

(** Use for the rightmost element to close a [^+] sequence, e.g.
    [pp1 ^+ pp2 ^+ last -| pp3] *)
val last : ('a list, 'a) t

module type STR = sig
  type t
  val of_string : string -> t
  val to_string : t -> string
end

(** Generates a string pp from a module with of/to string functions *)
val of_module :
  string -> (module STR with type t = 'a) -> (string, 'a) t

(** Parses to None on the empty list. Often combined with singleton
    ([opt (singleton _)]) *)
val opt : ('a list, 'b) t -> ('a list, 'b option) t

val default : 'a -> ('a option, 'a) t

(** [fallback p1 p2] is [p1], except that parsing is allowed to fail and will in
    that case try to parse through [p2]. Can be useful for backwards
    compatibility, but use with care *)
val fallback : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t

(** {3 Combinators to parse to a record from a list of (field name, field
    setter, field getter)} *)

(** Used to parse a single field of a record: ['a] on the left is the
    accumulator, or value of the record parsed so far. (in lens terms, [get]
    would be the print operation that extracts the field for the record, while
    [set] would be the parse operation that takes the input and record, and
    updates a given field in the record) *)
type ('a, 'value) field_parser = ('a * 'value option, 'a) t

(** Make a field parser from setter, getter and base pp. [cleanup] is an
    optional sanitisation function that is called on parsed elements
    before calling the setter. *)
val ppacc :
  ?cleanup:(pos:pos -> 'acc -> 'a -> 'a) ->
  ('a -> 'acc -> 'acc) ->
  ('acc -> 'a) ->
  ('value, 'a) t ->
  ('acc, 'value) field_parser

(** Same as [ppacc], but when the field may be unset in the record, i.e. the
    getter returns an option *)
val ppacc_opt :
  ?cleanup:(pos:pos -> 'acc -> 'a -> 'a) ->
  ('a -> 'acc -> 'acc) ->
  ('acc -> 'a option) ->
  ('value, 'a) t ->
  ('acc, 'value) field_parser

(** A field parser that ignores its argument *)
val ppacc_ignore : ('a, value) field_parser

val embed :
  ('a -> 'acc -> 'acc) -> ('acc -> 'a) -> ('a, 'value) field_parser ->
  ('acc, 'value) field_parser
