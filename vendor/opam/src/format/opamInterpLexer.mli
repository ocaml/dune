(**************************************************************************)
(*                                                                        *)
(*    Copyright 2016 MetaStack Solutions Ltd.                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** OPAM format variable interpolation processor *)

val main: (string -> unit) -> (string -> unit) -> Lexing.lexbuf -> unit
(** [main unquoted quoted lexbuf] fully processes the given lexbuf. Strings are
    applied to [unquoted] until a ["] or ["""] sequence is encountered when the
    content within the single or triple-quoted string is applied to [quoted]
    (note that the quote marks themselves are passed to [unquoted]). Within
    either string type, backslash is the escape character. *)
