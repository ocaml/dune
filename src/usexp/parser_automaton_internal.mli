(** Internal bits used by the generated automaton, not part of the public API *)

(** Internal state of the automaton *)
type 'a state

type 'a mode =
  | Single : Sexp_ast.t mode
  | Many   : Sexp_ast.t list mode

type stack
val empty_stack : stack

val new_state
  :  fname:string
  -> 'a mode
  -> 'a state

val mode : 'a state -> 'a mode

(** Number of characters fed to the parser *)
val offset : _ state -> int

(** Position in the text *)
val line   : _ state -> int
val column : _ state -> int

(** Whether there are some unclosed parentheses *)
val has_unclosed_paren : _ state -> bool

val set_error_state : _ state -> unit

val sexp_of_stack : stack -> Sexp_ast.t
val sexps_of_stack : stack -> Sexp_ast.t list

val automaton_state : _ state -> int

module Error : sig
  type t

  val position : t -> Lexing.position
  val message  : t -> string

  module Reason : sig
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

  val raise : _ state -> at_eof:bool -> Reason.t -> _
end

exception Parse_error of Error.t

val set_automaton_state : _ state -> int -> unit

(** Advance the position counters. [advance_eol] is for when we read a newline
    character. *)
val advance     : _ state -> unit
val advance_eol : _ state -> unit

(** Number of opened #| *)
val block_comment_depth : _ state -> int

type 'a action         = 'a state -> char -> stack -> stack
type 'a epsilon_action = 'a state ->         stack -> stack

(** Add a character to the atom buffer. [add_quoted_atom_char] does the same for quoted
    atoms *)
val add_atom_char : _ action
val add_quoted_atom_char : _ action

(** Add a character that just follows a '\\' and the '\\' itself if necessary. *)
val add_escaped : _ action

(** [escaped_value <- escaped_value * 10 + (char - '0')]

    These functions make the assumption that [char] is between '0' and '9'.
    [add_dec_escape_char] also assumes the result doesn't overflow. The automaton
    definition must make sure this is the case.

    [add_last_dec_escape_char] also adds the resulting character to the atom buffer.
*)
val add_dec_escape_char      : _ action
val add_last_dec_escape_char : _ action

(** Same but for quoted strings inside comments. Useful because it can fail. *)
val comment_add_last_dec_escape_char : _ action

(** Same as [add_dec_escape_char] but for hexadicemal escape sequences *)
val add_hex_escape_char      : _ action
val add_last_hex_escape_char : _ action

(** Ignore one more full sexp to come *)
val start_sexp_comment : _ action

(** Add the first char of an unquoted atom. *)
val add_first_char      : _ action
val start_quoted_string : _ action

(** Takes note of a control character in quoted atoms or the uninterpreted characters of
    comments, for which there is no corresponding [add_*] call (a backslash and the x in
    "\xff" or any character in a line comment).  This does not get called for the opening
    ([start_quoted_string]) or closing ([push_quoted_atom]) quotes themselves.
*)
val add_token_char : _ action

val opening      : _ action
val closing      : _ action
val push_quoted_atom : _ action

val start_block_comment : _ action
val end_block_comment   : _ action

val start_line_comment : _ action
val end_line_comment   : _ epsilon_action

val eps_push_atom      : _ epsilon_action
val eps_add_first_char_hash      : _ epsilon_action
val eps_eoi_check      : _ epsilon_action

val eps_add_escaped_cr : _ epsilon_action
