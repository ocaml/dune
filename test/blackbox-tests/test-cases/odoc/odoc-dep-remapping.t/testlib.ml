type t = { result : Odoc_parser.t; loc : Odoc_parser.Loc.span }

let parse text =
  let location = { Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 } in
  let result = Odoc_parser.parse_comment ~location ~text in
  let loc = { Odoc_parser.Loc.file = ""; start = { line = 1; column = 0 }; end_ = { line = 1; column = 0 } } in
  { result; loc }

let get_location t = t.loc
