type stack =
  | Empty
  | Open of Lexing.position * stack
  | Sexp of Sexp_ast.t * stack
let empty_stack = Empty

type 'a mode =
  | Single : Sexp_ast.t mode
  | Many   : Sexp_ast.t list mode

type 'a state =
  { mutable automaton_state : int
  ; mutable depth           : int
  ; (* Number of opened #| when parsing a block comment *)
    mutable block_comment_depth  : int
  ; (* Number of full sexp to ignore. This correspond to the number of consecutive #;.
       We don't have to track nested #; because they do not change the result. *)
    mutable ignoring       : int
  ; (* Only meaningful when [ignoring] > 0. Number of opened parentheses of the
       outermost sexp comment. *)
    mutable ignoring_depth : int
  ; (* When parsing an escape sequence of the form "\\NNN" or "\\XX", this accumulates
       the computed number *)
    mutable escaped_value  : int
  ; (* Buffer for accumulating atoms *)
    atom_buffer            : Buffer.t
  ; fname                  : string
  ; mutable full_sexps     : int
  ; mutable offset         : int (* global offset *)
  ; mutable line_number    : int
  ; mutable bol_offset     : int (* offset of beginning of line *)
  ; (* Starting positions of the current token *)
    mutable token_start_pos : Lexing.position
  ; mode                   : 'a mode
  }

(* these magic numbers are checked in gen_parser_automaton.ml:
   let () = assert (initial = 0)
   let () = assert (to_int Error = 1) *)
let initial_state = 0
let error_state   = 1

let new_state ~fname mode =
  { depth          = 0
  ; automaton_state = initial_state
  ; block_comment_depth  = 0
  ; ignoring       = 0
  ; ignoring_depth = 0
  ; escaped_value  = 0
  ; atom_buffer    = Buffer.create 128
  ; mode           = mode
  ; full_sexps     = 0
  ; offset         = 0
  ; line_number    = 1
  ; bol_offset     = 0
  ; fname          = fname
  ; token_start_pos = { pos_fname = fname; pos_cnum = 0; pos_lnum = 1; pos_bol = 0 }
  }

let mode t = t.mode

let offset t = t.offset
let line   t = t.line_number
let column t = t.offset - t.bol_offset

let position t =
  { Lexing.
    pos_fname = t.fname
  ; pos_cnum  = t.offset
  ; pos_lnum  = t.line_number
  ; pos_bol   = t.bol_offset
  }

let has_unclosed_paren state = state.depth > 0

let set_error_state state = state.automaton_state <- error_state

let sexp_of_stack : stack -> Sexp_ast.t = function
  | Sexp (sexp, Empty) -> sexp
  | _ -> failwith "Parser_automaton.sexp_of_stack"

let sexps_of_stack =
  let rec loop acc : stack -> Sexp_ast.t list = function
    | Empty -> acc
    | Open _ -> failwith "Parser_automaton.sexps_of_stack"
    | Sexp (sexp, stack) -> loop (sexp :: acc) stack
  in
  fun stack -> loop [] stack

let automaton_state state = state.automaton_state

module Error = struct
  type t =
    { position       : Lexing.position
    ; message        : string
    }

  exception Parse_error of t

  module Reason = struct
    (* To be kept in sync with the Error module in gen/gen_parser_automaton.ml *)
    type t =
      | Unexpected_char_parsing_hex_escape
      | Unexpected_char_parsing_dec_escape
      | Unterminated_quoted_string
      | Unterminated_block_comment
      | Escape_sequence_out_of_range
      | Unclosed_paren
      | Too_many_sexps
      | Closed_paren_without_opened
      | Comment_token_in_unquoted_atom
      | Sexp_comment_without_sexp
      | Unexpected_character_after_cr
      | No_sexp_found_in_input
      | Automaton_in_error_state
  end

  let raise state ~at_eof (reason : Reason.t) =
    set_error_state state;
    let message =
      (* These messages where choosen such that we can build the various Sexplib parsing
         functions on top of Parsexp and keep the same exceptions.

         At the time of writing this, a simple layer on top of parsexp to implement the
         sexplib API is passing all the sexplib tests.

         Note that parsexp matches the semantic of Sexp.parse which is slightly
         different from the ocamllex/ocamlyacc based parser of Sexplib. The latter one
         is less tested and assumed to be less used. *)
      match reason with
      | Unexpected_char_parsing_hex_escape ->
        "unterminated hexadecimal escape sequence"
      | Unexpected_char_parsing_dec_escape ->
        "unterminated decimal escape sequence"
      | Unterminated_quoted_string ->
        "unterminated quoted string"
      | Unterminated_block_comment ->
        "unterminated block comment"
      | Escape_sequence_out_of_range ->
        "escape sequence in quoted string out of range"
      | Unclosed_paren ->
        "unclosed parentheses at end of input"
      | Too_many_sexps ->
        "s-expression followed by data"
      | Closed_paren_without_opened ->
        "unexpected character: ')'"
      | Comment_token_in_unquoted_atom ->
        if Buffer.contents state.atom_buffer = "|" then
          "illegal end of comment"
        else
          "comment tokens in unquoted atom"
      | Sexp_comment_without_sexp ->
        "unterminated sexp comment"
      | Unexpected_character_after_cr ->
        if at_eof then
          "unexpected end of input after carriage return"
        else
          "unexpected character after carriage return"
      | No_sexp_found_in_input ->
        "no s-expression found in input"
      | Automaton_in_error_state ->
        failwith "Parsexp.Parser_automaton: parser is dead"
    in
    let position = position state in
    raise (Parse_error { position; message })

  let position       t = t.position
  let message        t = t.message
end

exception Parse_error = Error.Parse_error

type 'a action         = 'a state -> char -> stack -> stack
type 'a epsilon_action = 'a state ->         stack -> stack

let current_pos ?(delta=0) state : Lexing.position =
  let offset = state.offset + delta in
  { pos_fname = state.fname
  ; pos_lnum   = state.line_number
  ; pos_cnum   = offset
  ; pos_bol    = state.bol_offset
  }

let set_automaton_state state x = state.automaton_state <- x

let advance state = state.offset <- state.offset + 1

let advance_eol state =
  let newline_offset = state.offset in
  state.offset <- newline_offset + 1;
  state.bol_offset <- state.offset;
  state.line_number <- state.line_number + 1

let block_comment_depth state = state.block_comment_depth

let add_token_char _state _char stack = stack

let add_atom_char state c stack =
  Buffer.add_char state.atom_buffer c;
  stack

let add_quoted_atom_char state c stack =
  Buffer.add_char state.atom_buffer c;
  add_token_char state c stack

let check_new_sexp_allowed : type a. a state -> unit = fun state ->
  let is_single = match state.mode with Single -> true | _ -> false in
  if is_single && state.full_sexps > 0 && state.ignoring = 0 then
    Error.raise state ~at_eof:false Too_many_sexps

let add_first_char state char stack =
  check_new_sexp_allowed state;
  Buffer.add_char state.atom_buffer char;
  (* For non-quoted atoms, we save both positions at the end. We can always determine the
     start position from the end position and the atom length for non-quoted atoms.

     Doing it this way allows us to detect single characater atoms for which we need to
     save the position twice. *)
  stack

let eps_add_first_char_hash state stack =
  check_new_sexp_allowed state;
  Buffer.add_char state.atom_buffer '#';
  stack

let start_quoted_string state _char stack =
  check_new_sexp_allowed state;
  state.token_start_pos <- current_pos state;
  stack

let add_escaped state c stack =
  let c' =
    match c with
    | 'n' -> '\n'
    | 'r' -> '\r'
    | 'b' -> '\b'
    | 't' -> '\t'
    | '\\' | '\'' | '"' -> c
    | _ -> Buffer.add_char state.atom_buffer '\\'; c
  in
  Buffer.add_char state.atom_buffer c';
  add_token_char state c stack

let eps_add_escaped_cr state stack =
  Buffer.add_char state.atom_buffer '\r';
  stack

let dec_val c = Char.code c - Char.code '0'

let hex_val c =
  match c with
  | '0'..'9' -> Char.code c - Char.code '0'
  | 'a'..'f' -> Char.code c - Char.code 'a' + 10
  | _        -> Char.code c - Char.code 'A' + 10

let add_dec_escape_char state c stack =
  state.escaped_value <- state.escaped_value * 10 + dec_val c;
  add_token_char state c stack

let add_last_dec_escape_char state c stack =
  let value = state.escaped_value * 10 + dec_val c in
  state.escaped_value <- 0;
  if value > 255 then Error.raise state ~at_eof:false Escape_sequence_out_of_range;
  Buffer.add_char state.atom_buffer (Char.unsafe_chr value);
  add_token_char state c stack

let comment_add_last_dec_escape_char state c stack =
  let value = state.escaped_value * 10 + dec_val c in
  state.escaped_value <- 0;
  if value > 255 then Error.raise state ~at_eof:false Escape_sequence_out_of_range;
  add_token_char state c stack

let add_hex_escape_char state c stack =
  state.escaped_value <- (state.escaped_value lsl 4) lor hex_val c;
  add_token_char state c stack

let add_last_hex_escape_char state c stack =
  let value = (state.escaped_value lsl 4) lor hex_val c in
  state.escaped_value <- 0;
  Buffer.add_char state.atom_buffer (Char.unsafe_chr value);
  add_token_char state c stack

let opening state _char stack =
  check_new_sexp_allowed state;
  state.depth <- state.depth + 1;
  if state.ignoring = 0 then
    Open (current_pos state, stack)
  else
    stack

let sexp_or_comment_added ~is_comment state stack ~delta:_ =
  if state.ignoring lor state.depth <> 0 then
    stack
  else begin
    if not is_comment then state.full_sexps <- state.full_sexps + 1;
    stack
  end

let sexp_added state stack ~delta =
  let stack = sexp_or_comment_added ~is_comment:false state stack ~delta in
  if state.ignoring <> 0 && state.ignoring_depth = state.depth then begin
    state.ignoring <- state.ignoring - 1;
    stack
  end else stack

let rec make_list stop acc : stack -> stack = function
  | Empty               -> assert false
  | Open (start, stack) -> Sexp (List ({ start; stop }, acc), stack)
  | Sexp (sexp, stack)  -> make_list stop (sexp :: acc) stack

let closing state _char stack =
  if state.depth > 0 then begin
    let stack =
      if state.ignoring = 0 then
        make_list (current_pos state ~delta:1) [] stack
      else
        stack
    in
    if state.ignoring <> 0 && state.ignoring_depth = state.depth then
      Error.raise state ~at_eof:false Sexp_comment_without_sexp;
    state.depth <- state.depth - 1;
    sexp_added state stack ~delta:1
  end else
    Error.raise state ~at_eof:false Closed_paren_without_opened

let make_loc ?(delta=0) state : Sexp_ast.Loc.t =
  { start = state.token_start_pos
  ; stop  = current_pos state ~delta
  }

let eps_push_atom state stack =
  let str = Buffer.contents state.atom_buffer in
  Buffer.clear state.atom_buffer;
  let stack =
    if state.ignoring = 0 then
      Sexp (Atom (make_loc state, A str), stack)
    else
      stack
  in
  sexp_added state stack ~delta:0

let push_quoted_atom state _char stack =
  let str = Buffer.contents state.atom_buffer in
  Buffer.clear state.atom_buffer;
  let stack =
    if state.ignoring = 0 then
      Sexp (Quoted_string (make_loc state ~delta:1, str), stack)
    else
      stack
  in
  sexp_added state stack ~delta:1

let start_sexp_comment state _char stack =
  if state.ignoring = 0 || state.ignoring_depth = state.depth then begin
    state.ignoring <- state.ignoring + 1;
    state.ignoring_depth <- state.depth
  end;
  stack

let start_block_comment state _char stack =
  state.block_comment_depth <- state.block_comment_depth + 1;
  stack

let end_block_comment state _char stack =
  state.block_comment_depth <- state.block_comment_depth - 1;
  stack

let start_line_comment _state _char stack = stack
let   end_line_comment _state       stack = stack

let eps_eoi_check : type a. a state -> stack -> stack = fun state stack ->
  if state.depth      > 0 then Error.raise state ~at_eof:true Unclosed_paren;
  if state.ignoring   > 0 then Error.raise state ~at_eof:true Sexp_comment_without_sexp;
  if state.full_sexps = 0 then (
    match state.mode with
    | Many -> ()
    | Single ->
      Error.raise state ~at_eof:true No_sexp_found_in_input
  );
  stack
