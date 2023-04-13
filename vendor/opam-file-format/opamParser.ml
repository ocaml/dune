(**************************************************************************)
(*                                                                        *)
(*    Copyright 2016-2018 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Generic glue functions *)
let parse_from_string parse_fun str filename =
  let lexbuf = Lexing.from_string str in
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
  parse_fun OpamLexer.token lexbuf

let parse_from_channel parse_fun ic filename =
  let lexbuf = Lexing.from_channel ic in
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
  parse_fun OpamLexer.token lexbuf

let parse_from_file parse_fun filename =
  let ic = open_in_bin filename in
  try
    let r = parse_from_channel parse_fun ic filename in
    close_in ic;
    r
  with e -> close_in ic; raise e

(** file parsers *)
let main' main filename lexer lexbuf = main lexer lexbuf filename
let string main str filename = parse_from_string (main' main filename) str filename
let channel main ic filename = parse_from_channel (main' main filename) ic filename
let file main filename = parse_from_file (main' main filename) filename

module FullPos = struct

  open OpamParserTypes.FullPos

  (** raw parser entry points *)
  let main = OpamBaseParser.main
  let value = OpamBaseParser.value

  (** file parsers *)
  let string = string main
  let channel = channel main
  let file = file main

  (** value parsers *)
  let value_from_string = parse_from_string value
  let value_from_channel = parse_from_channel value
  let value_from_file = parse_from_file value

  (* Functions to transform simple pos to full pos *)
  module S = OpamParserTypes

  let to_pos e = (e.pos.filename, fst e.pos.start, snd e.pos.start)

  let rec to_value v =
    match v.pelem with
    | Bool b -> S.Bool (to_pos v, b)
    | Int i -> S.Int (to_pos v, i)
    | String s -> S.String (to_pos v, s)
    | Relop (r,v,v') -> S.Relop (to_pos r, r.pelem, to_value v, to_value v')
    | Prefix_relop (r,v) -> S.Prefix_relop (to_pos r, r.pelem, to_value v)
    | Logop (l,v,v') -> S.Logop (to_pos l, l.pelem, to_value v, to_value v')
    | Pfxop (p,v) -> S.Pfxop (to_pos p, p.pelem, to_value v)
    | Ident s -> S.Ident (to_pos v, s)
    | List l -> S.List (to_pos l, List.map to_value l.pelem)
    | Group g -> S.Group (to_pos g, List.map to_value g.pelem)
    | Option (v,vl) -> S.Option (to_pos vl, to_value v, List.map to_value vl.pelem)
    | Env_binding (v,o,v') -> S.Env_binding (to_pos o, to_value v, o.pelem, to_value v')

  let rec to_section s =
    { S.section_kind = s.section_kind.pelem;
      S.section_name =
        (match s.section_name with
         | Some n -> Some n.pelem
         | None -> None);
      S.section_items = List.map to_item s.section_items.pelem
    }
  and to_item i =
    match i.pelem with
    | Section s -> S.Section (to_pos s.section_kind, to_section s)
    | Variable (s, v) -> S.Variable (to_pos s, s.pelem, to_value v)

  let to_opamfile o =
    { S.file_contents = List.map to_item o.file_contents;
      S.file_name = o.file_name
    }

end

(** raw parser entry points *)
let main f l fd = FullPos.to_opamfile (OpamBaseParser.main f l fd)
let value f l = FullPos.to_value (OpamBaseParser.value f l)

(** file parsers *)
let string = string main
let channel = channel main
let file = file main

(** value parsers *)
let value_from_string = parse_from_string value
let value_from_channel = parse_from_channel value
let value_from_file = parse_from_file value
