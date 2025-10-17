{
open Import

type 'command block =
  | Command of Loc.t * 'command
  | Comment of string list
}

let nonspace = [^' ' '\n']
let not_nl  = [^'\n']

rule eol = parse
  | '\n' { Lexing.new_line lexbuf; true }
  | eof  { false }

and block = parse
  | eof { None }
  | "  $ " ([^'\n']* as str)
    { let start = Lexing.lexeme_start_p lexbuf in
      let start = { start with pos_cnum = start.pos_cnum + 2 } in
      let stop0 = Lexing.lexeme_end_p lexbuf in
      match eol lexbuf with
      | true  -> Some (command_cont start stop0 [ str ] lexbuf)
      | false -> Some (Command (Loc.create ~start ~stop:stop0, [ str ])) }
  | "  > " ([^'\n']* as str)
    { ignore (eol lexbuf);
      comment [ "  > " ^ str ] lexbuf }
  | "  >"
    { ignore (eol lexbuf);
      comment [ "  >" ] lexbuf }
  | "  " [^'\n']*
    { ignore (eol lexbuf);
      output [] lexbuf }
  | ' ' ((nonspace not_nl*) as rest)
    { ignore (eol lexbuf);
      comment [ " " ^ rest ] lexbuf }
  | ' ' '\n'
    { Lexing.new_line lexbuf;
      comment [ " " ] lexbuf }
  | ' '
    { comment [ " " ] lexbuf }
  | '\n'
    { Lexing.new_line lexbuf;
      comment [ "" ] lexbuf }
  | nonspace not_nl* as str
    { ignore (eol lexbuf);
      comment [str] lexbuf }

and comment acc = parse
  | eof
    { match acc with
      | [] -> None
      | _ -> Some (Comment (List.rev acc)) }
  | ' ' ((nonspace not_nl*) as rest)
    { ignore (eol lexbuf);
      comment ((" " ^ rest) :: acc) lexbuf }
  | ' ' '\n'
    { Lexing.new_line lexbuf;
      comment (" " :: acc) lexbuf }
  | '\n'
    { Lexing.new_line lexbuf;
      comment ("" :: acc) lexbuf }
  | nonspace not_nl* as str
    { ignore (eol lexbuf);
      comment (str :: acc) lexbuf }
  | "" { Some (Comment (List.rev acc)) }

and output maybe_comment = parse
  | eof
    { match maybe_comment with
      | [] -> None
      | l -> Some (Comment (List.rev l)) }
  | "  $ " ([^'\n']* as str)
    { let start = Lexing.lexeme_start_p lexbuf in
      let start = { start with pos_cnum = start.pos_cnum + 2 } in
      let stop0 = Lexing.lexeme_end_p lexbuf in
      match eol lexbuf with
      | true  -> Some (command_cont start stop0 [ str ] lexbuf)
      | false -> Some (Command (Loc.create ~start ~stop:stop0, [ str ])) }
  | ' ' ((nonspace not_nl*) as rest)
    { match eol lexbuf with
      | true  -> output ((" " ^ rest) :: maybe_comment) lexbuf
      | false -> Some (Comment (List.rev ((" " ^ rest) :: maybe_comment))) }
  | ' ' '\n'
    { Lexing.new_line lexbuf;
      output (" " :: maybe_comment) lexbuf }
  | "  " [^'\n']*
    { ignore (eol lexbuf);
      output maybe_comment lexbuf }
  | ""
    { match maybe_comment with
      | [] -> block lexbuf
      | l -> comment l lexbuf }

and command_cont start last_stop acc = parse
  | "  > " ([^'\n']* as str)
    { let stop0 = Lexing.lexeme_end_p lexbuf in
      match eol lexbuf with
      | true  -> command_cont start stop0 (str :: acc) lexbuf
      | false -> Command (Loc.create ~start ~stop:stop0, List.rev (str :: acc)) }
  | "  >"
    { let stop0 = Lexing.lexeme_end_p lexbuf in
      match eol lexbuf with
      | true  -> command_cont start stop0 ("" :: acc) lexbuf
      | false -> Command (Loc.create ~start ~stop:stop0, List.rev ("" :: acc)) }
  | ""
    { Command (Loc.create ~start ~stop:last_stop, List.rev acc) }
