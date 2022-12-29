(**************************************************************************)
(*                                                                        *)
(*    Copyright 2016-2020 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** opam format parser *)

open OpamParserTypes

(** [OpamParser] transitional module with full position types *)
module FullPos : sig

  open OpamParserTypes.FullPos

  (** {2 Raw OpamBaseParser entry points } *)

  (**
      Providing a custom [lexbuf] argument allows you, for example, to set the
      initial lexing position. For the first argument, you may use the
      {!OpamLexer.token} lexing function:

     {[
       let lexbuf = Lexing.from_string input in
       lexbuf.Lexing.lex_curr_p <- current_position;
       OpamParser.value OpamLexer.token lexbuf
     ]}
  *)

  val main:
    (Lexing.lexbuf  -> OpamBaseParser.token) ->
    Lexing.lexbuf -> file_name -> opamfile
  (** Principal parser: given a lexbuf and the filename it was read from, returns
      an {!OpamParserTypes.FullPos.opamfile} record parsed from it. *)

  val value:
    (Lexing.lexbuf  -> OpamBaseParser.token) ->
    Lexing.lexbuf -> value
  (** Lower-level function just returning a single
      {!OpamParserTypes.FullPos.value} from a given lexer. *)

  (** {2 File parsers } *)

  val string: string -> file_name -> opamfile
  (** Parse the content of a file already read to a string. Note that for
      CRLF-detection to work on Windows, it is necessary to read the original file
      using binary mode on Windows! *)

  val channel: in_channel -> file_name -> opamfile
  (** Parse the content of a file from an already-opened channel. Note that for
      CRLF-detection to work on Windows, it is necessary for the channel to be
      in binary mode! *)

  val file: file_name -> opamfile
  (** Parse the content of a file. The file is opened in binary mode, so
      CRLF-detection works on all platforms. *)

  (** {2 [value] parsers } *)

  val value_from_string: string -> file_name -> value
  (** Parse the first value in the given string. [file_name] is used for lexer
      positions. *)

  val value_from_channel: in_channel -> file_name -> value
  (** Parse the first value from the given channel. [file_name] is used for
      lexer positions. *)

  val value_from_file: file_name -> value
  (** Parse the first value from the given file. *)

  (** {2 Conversion functions, from full position to simple position (legacy)} *)

  val to_value: value -> OpamParserTypes.value
  val to_section: opamfile_section -> OpamParserTypes.opamfile_section
  val to_item: opamfile_item -> OpamParserTypes.opamfile_item
  val to_opamfile: opamfile -> OpamParserTypes.opamfile

end

val main:
  (Lexing.lexbuf  -> OpamBaseParser.token) ->
  Lexing.lexbuf -> file_name -> opamfile
[@@ocaml.deprecated "Use OpamParser.FullPos.main instead."]

val value:
  (Lexing.lexbuf  -> OpamBaseParser.token) ->
  Lexing.lexbuf -> value
[@@ocaml.deprecated "Use OpamParser.FullPos.value instead."]

val string: string -> file_name -> opamfile
[@@ocaml.deprecated "Use OpamParser.FullPos.string instead."]

val channel: in_channel -> file_name -> opamfile
[@@ocaml.deprecated "Use OpamParser.FullPos.channel instead."]
val file: file_name -> opamfile
[@@ocaml.deprecated "Use OpamParser.FullPos.file instead."]

val value_from_string: string -> file_name -> value
[@@ocaml.deprecated "Use OpamParser.FullPos.value_from_string instead."]

val value_from_channel: in_channel -> file_name -> value
[@@ocaml.deprecated "Use OpamParser.FullPos.value_from_channel instead."]

val value_from_file: file_name -> value
[@@ocaml.deprecated "Use OpamParser.FullPos.value_from_file instead."]
