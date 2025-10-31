{
open Import

type 'command block =
  | Command of 'command
  | Comment of string list

(* Gets position before consuming newline *)
let pos_before_newline lexbuf =
  let pos = Lexing.lexeme_end_p lexbuf in
  { pos with pos_cnum = pos.pos_cnum - 1 }

(* Creates comment from string list accumulator *)
let create_comment_from_acc ~start ~stop acc =
  match acc with
  | [] -> None
  | _ -> Some (Loc.create ~start ~stop, Comment (List.rev acc))

(* Helper for extracting start/stop positions *)
let with_span lexbuf f =
  let start = Lexing.lexeme_start_p lexbuf in
  let stop = Lexing.lexeme_end_p lexbuf in
  f ~start ~stop

(* Forward declarations for lexer rules *)
let eol_fdecl = Fdecl.create Dyn.opaque
let command_cont_fdecl = Fdecl.create Dyn.opaque

(* Helper for position extraction + eol + recursive call *)
let with_span_eol lexbuf content next_rule =
  with_span lexbuf (fun ~start ~stop ->
    ignore (Fdecl.get eol_fdecl lexbuf);
    next_rule start stop content lexbuf)

(* Handles command continuation with eol checking *)
let handle_command_continuation ~start ~content ~acc lexbuf =
  let stop = Lexing.lexeme_end_p lexbuf in
  match Fdecl.get eol_fdecl lexbuf with
  | true  -> Fdecl.get command_cont_fdecl start stop (content :: acc) lexbuf
  | false -> (Loc.create ~start ~stop, Command (List.rev (content :: acc)))

(* Processes "  $ " command start pattern *)
let process_command_start str lexbuf =
  let start =
    let pos = Lexing.lexeme_start_p lexbuf in
    { pos with pos_cnum = pos.pos_cnum + 2 }
  in
  let stop = Lexing.lexeme_end_p lexbuf in
  match Fdecl.get eol_fdecl lexbuf with
  | true  -> Some (Fdecl.get command_cont_fdecl start stop [ str ] lexbuf)
  | false -> Some (Loc.create ~start ~stop, Command [ str ])
}

let nonspace = [^' ' '\n']
let not_nl  = [^'\n']

rule eol = parse
  | '\n' { Lexing.new_line lexbuf; true }
  | eof  { false }

and block = parse
  | eof { None }
  | "  $ " ([^'\n']* as str)
    { process_command_start str lexbuf }
  | "  > " ([^'\n']* as str)
    { with_span_eol lexbuf [ "  > " ^ str ] comment }
  | "  >"
    { with_span_eol lexbuf [ "  >" ] comment }
  | "  " [^'\n']*
    { with_span_eol lexbuf [] (fun start _ content lexbuf -> output start content lexbuf) }
  | ' ' ((nonspace not_nl*) as rest)
    { with_span_eol lexbuf [ " " ^ rest ] comment }
  | ' ' '\n'
    { let start = Lexing.lexeme_start_p lexbuf in
      let stop = pos_before_newline lexbuf in
      Lexing.new_line lexbuf;
      comment start stop [ " " ] lexbuf }
  | ' '
    { with_span lexbuf (fun ~start ~stop -> comment start stop [ " " ] lexbuf) }
  | '\n'
    { let start = Lexing.lexeme_start_p lexbuf in
      let stop = Lexing.lexeme_start_p lexbuf in
      Lexing.new_line lexbuf;
      comment start stop [ "" ] lexbuf }
  | nonspace not_nl* as str
    { with_span_eol lexbuf [str] comment }

and comment start last_content_stop acc = parse
  | eof
    { create_comment_from_acc ~start ~stop:last_content_stop acc }
  | ' ' ((nonspace not_nl*) as rest)
    { with_span_eol lexbuf [(" " ^ rest)] (fun _start stop content lexbuf ->
        comment start stop (content @ acc) lexbuf) }
  | ' ' '\n'
    { let content_stop = pos_before_newline lexbuf in
      Lexing.new_line lexbuf;
      comment start content_stop (" " :: acc) lexbuf }
  | '\n'
    { let content_stop = Lexing.lexeme_start_p lexbuf in
      Lexing.new_line lexbuf;
      comment start content_stop ("" :: acc) lexbuf }
  | nonspace not_nl* as str
    { with_span_eol lexbuf [str] (fun _start stop content lexbuf ->
        comment start stop (content @ acc) lexbuf) }
  | "" { create_comment_from_acc ~start ~stop:last_content_stop acc }


and output block_start maybe_comment = parse
  | eof
    { create_comment_from_acc ~start:block_start ~stop:(Lexing.lexeme_start_p lexbuf) maybe_comment }
  | "  $ " ([^'\n']* as str)
    { process_command_start str lexbuf }
  | ' ' ((nonspace not_nl*) as rest)
    { match eol lexbuf with
      | true  -> output block_start ((" " ^ rest) :: maybe_comment) lexbuf
      | false ->
        Some (Loc.create ~start:block_start ~stop:(Lexing.lexeme_start_p lexbuf)
             , Comment (List.rev ((" " ^ rest) :: maybe_comment))) }
  | ' ' '\n'
    { Lexing.new_line lexbuf;
      output block_start (" " :: maybe_comment) lexbuf }
  | "  " [^'\n']*
    { with_span_eol lexbuf [] (fun _start _stop _content lexbuf ->
        output block_start maybe_comment lexbuf) }
  | ""
    { match maybe_comment with
      | [] -> block lexbuf
      | l -> comment block_start (Lexing.lexeme_start_p lexbuf) l lexbuf }

and command_cont start last_stop acc = parse
  | "  > " ([^'\n']* as str)
    { handle_command_continuation ~start ~content:str ~acc lexbuf }
  | "  >"
    { handle_command_continuation ~start ~content:"" ~acc lexbuf }
  | ""
    { (Loc.create ~start ~stop:last_stop, Command (List.rev acc)) }

{
let () =
  Fdecl.set eol_fdecl eol;
  Fdecl.set command_cont_fdecl command_cont
}
