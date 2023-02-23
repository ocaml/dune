(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2016 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

{

type token =
  | WORD of string
  | NEWLINE
  | EOF

let word = Buffer.create 57

}

let normalchar = [^' ' '\t' '\r' '\n' '\\']

rule main = parse
| '\n' | "\r\n"
               { Lexing.new_line lexbuf; NEWLINE }
| [' ' '\t']+  { main lexbuf }
| ('@' normalchar*) as w '\\'
               { Buffer.reset word ; Buffer.add_string word w; escaped lexbuf }
| ('@' normalchar*) as w
               { if w = "@" then WORD "" else WORD w }
| (normalchar* as w) '\\'
               { Buffer.reset word ; Buffer.add_string word w; escaped lexbuf }
| (normalchar+ as w)
               { WORD w }
| eof          { EOF }

and escaped = parse
| (_ normalchar*) as w '\\'
               { Buffer.add_string word w; escaped lexbuf }
| (_ normalchar*) as w
               { Buffer.add_string word w; WORD (Buffer.contents word) }

{

let main lexbuf =
  let rec aux lines words =
    match main lexbuf with
    | WORD s -> aux lines (s::words)
    | NEWLINE ->
      let lines = if words = [] then lines else List.rev words::lines in
      aux lines []
    | EOF ->
      let lines = if words = [] then lines else List.rev words::lines in
      List.rev lines
  in
  aux [] []

}
