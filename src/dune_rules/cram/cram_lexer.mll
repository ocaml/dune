{
open Import

type 'command block =
  | Command of 'command
  | Comment of string list

(* Creates location from lexbuf *)
let loc lexbuf =
  Loc.create ~start:(Lexing.lexeme_start_p lexbuf) ~stop:(Lexing.lexeme_end_p lexbuf)
[@@inline]

(* Adjusts the start position by an offset *)
let adjust_start_cnum offset loc =
  let start = Loc.start loc in
  let start = { start with pos_cnum = start.pos_cnum + offset } in
  Loc.set_start loc start
[@@inline]

(* Creates location for "  $ " pattern, adjusting past the "  $ " prefix *)
let loc_of_dollar lexbuf =
  loc lexbuf |> adjust_start_cnum 2
[@@inline]

(* Gets position before consuming newline *)
let pos_before_newline lexbuf =
  let pos = Lexing.lexeme_end_p lexbuf in
  { pos with pos_cnum = pos.pos_cnum - 1 }
[@@inline]

(* Creates comment from string list accumulator *)
let create_comment_from_acc loc acc =
  match acc with
  | [] -> None
  | _ -> Some (loc, Comment (List.rev acc))
[@@inline]
}

let nonspace = [^' ' '\n']
let not_nl  = [^'\n']

rule block = parse
  | eof { None }
  | "  $ " ([^'\n']* as str)
    { eol_then_command_or_finish (loc_of_dollar lexbuf) str lexbuf }
  | "  > " ([^'\n']* as str)
    { after_comment_line (loc lexbuf) [ "  > " ^ str ] lexbuf }
  | "  >"
    { after_comment_line (loc lexbuf) [ "  >" ] lexbuf }
  | "  " [^'\n']*
    { after_output_line (loc lexbuf) lexbuf }
  | ' ' ((nonspace not_nl*) as rest)
    { after_comment_line (loc lexbuf) [ " " ^ rest ] lexbuf }
  | ' ' '\n'
    { let loc = loc lexbuf in
      let loc = Loc.set_stop loc (pos_before_newline lexbuf) in
      Lexing.new_line lexbuf;
      comment loc [ " " ] lexbuf }
  | ' '
    { comment (loc lexbuf) [ " " ] lexbuf }
  | '\n'
    { let loc = loc lexbuf in
      let loc = Loc.set_stop loc (Loc.start loc) in
      Lexing.new_line lexbuf;
      comment loc [ "" ] lexbuf }
  | nonspace not_nl* as str
    { after_comment_line (loc lexbuf) [str] lexbuf }

and after_comment_line loc content = parse
  | '\n' { Lexing.new_line lexbuf; comment loc content lexbuf }
  | eof  { comment loc content lexbuf }

and after_output_line loc = parse
  | '\n' { Lexing.new_line lexbuf; output loc [] lexbuf }
  | eof  { output loc [] lexbuf }

and eol_then_command_or_finish loc str = parse
  | '\n' { Lexing.new_line lexbuf; Some (command_cont loc [ str ] lexbuf) }
  | eof  { Some (loc, Command [ str ]) }

and comment loc acc = parse
  | eof
    { create_comment_from_acc loc acc }
  | ' ' ((nonspace not_nl*) as rest)
    { let loc = Loc.set_stop loc (Lexing.lexeme_end_p lexbuf) in
      after_comment_line_in_comment loc (" " ^ rest) acc lexbuf }
  | ' ' '\n'
    { let loc = Loc.set_stop loc (pos_before_newline lexbuf) in
      Lexing.new_line lexbuf;
      comment loc (" " :: acc) lexbuf }
  | '\n'
    { let loc = Loc.set_stop loc (Lexing.lexeme_start_p lexbuf) in
      Lexing.new_line lexbuf;
      comment loc ("" :: acc) lexbuf }
  | nonspace not_nl* as str
    { let loc = Loc.set_stop loc (Lexing.lexeme_end_p lexbuf) in
      after_comment_line_in_comment loc str acc lexbuf }
  | "" { create_comment_from_acc loc acc }

and after_comment_line_in_comment loc str acc = parse
  | '\n' { Lexing.new_line lexbuf; comment loc (str :: acc) lexbuf }
  | eof  { comment loc (str :: acc) lexbuf }

and output loc maybe_comment = parse
  | eof
    { let loc = Loc.set_stop loc (Lexing.lexeme_start_p lexbuf) in
      create_comment_from_acc loc maybe_comment }
  | "  $ " ([^'\n']* as str)
    { eol_then_command_or_finish (loc_of_dollar lexbuf) str lexbuf }
  | ' ' ((nonspace not_nl*) as rest)
    { after_comment_line_in_output loc ((" " ^ rest) :: maybe_comment) lexbuf }
  | ' ' '\n'
    { Lexing.new_line lexbuf;
      output loc (" " :: maybe_comment) lexbuf }
  | "  " [^'\n']*
    { after_output_line_in_output loc maybe_comment lexbuf }
  | ""
    { match maybe_comment with
      | [] -> block lexbuf
      | l ->
        let loc = Loc.set_stop loc (Lexing.lexeme_start_p lexbuf) in
        comment loc l lexbuf }

and after_output_line_in_output loc maybe_comment = parse
  | '\n' { Lexing.new_line lexbuf; output loc maybe_comment lexbuf }
  | eof  { output loc maybe_comment lexbuf }

and after_comment_line_in_output loc maybe_comment = parse
  | '\n' { Lexing.new_line lexbuf; comment loc maybe_comment lexbuf }
  | eof  {
    let loc = Loc.set_stop loc (Lexing.lexeme_start_p lexbuf) in
    Some (loc, Comment (List.rev maybe_comment)) }

and command_cont loc acc = parse
  | "  > " ([^'\n']* as str)
    { let loc = Loc.set_stop loc (Lexing.lexeme_end_p lexbuf) in
      eol_then_continue_or_finish loc str acc lexbuf }
  | "  >"
    { let loc = Loc.set_stop loc (Lexing.lexeme_end_p lexbuf) in
      eol_then_continue_or_finish loc "" acc lexbuf }
  | ""
    { (loc, Command (List.rev acc)) }

and eol_then_continue_or_finish loc content acc = parse
  | '\n' { Lexing.new_line lexbuf; command_cont loc (content :: acc) lexbuf }
  | eof  { (loc, Command (List.rev (content :: acc))) }
