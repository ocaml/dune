(**************************************************************************)
(*                                                                        *)
(*    Copyright 2016 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamParserTypes

(** Raw OpamBaseParser entry points;

    Providing a custom [lexbuf] argument allows you, for example, to
    set the initial lexing position. For the first argument, you may
    use the {!OpamLexer.token} lexing function:

{[
    let lexbuf = Lexing.from_string input in
    lexbuf.Lexing.lex_curr_p <- current_position;
    OpamParser.value OpamLexer.token lexbuf
]}
*)
val main:
  (Lexing.lexbuf  -> OpamBaseParser.token) ->
  Lexing.lexbuf -> file_name -> opamfile
val value:
  (Lexing.lexbuf  -> OpamBaseParser.token) ->
  Lexing.lexbuf -> value

(** file parsers *)
val string: string -> file_name -> opamfile
val channel: in_channel -> file_name -> opamfile
val file: file_name -> opamfile

(** value parsers *)
val value_from_string: string -> file_name -> value
val value_from_channel: in_channel -> file_name -> value
val value_from_file: file_name -> value
