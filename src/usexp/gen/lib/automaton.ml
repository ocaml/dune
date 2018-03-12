(* Abstract version of the parsing automaton.

   It is used in two places:

   - to define the automaton. At runtime, we only use integer for states and a table of
     functions for transitions

   - for tests
*)

open Base

module State = struct
  module Quoted_string = struct
    type t =
      | Normal
      | After_backslash
      | After_backslash_cr
      | After_backslash_digit
      | After_backslash_2digits
      | After_backslash_x
      | After_backslash_x_hex
      | Ignoring_blanks
    [@@deriving enumerate, compare, sexp_of]
  end

  module Block_comment = struct
    type t =
      | Normal
      | After_pipe
      | After_hash
      | Quoted_string of Quoted_string.t
    [@@deriving enumerate, compare, sexp_of]
  end

  module Unquoted_string = struct
    type t =
      | Normal
      | After_hash
      | After_pipe
    [@@deriving enumerate, compare, sexp_of]
  end

  type t =
    | Whitespace
    | Error
    | After_cr
    | Unquoted_string of Unquoted_string.t
    | Line_comment
    | After_hash
    | Quoted_string of Quoted_string.t
    | Block_comment of Block_comment.t
  [@@deriving enumerate, compare, sexp_of]

  let to_int t =
    let rec loop i t l =
      match l with
      | [] -> assert false
      | x :: l -> if [%compare.equal: t] t x then i else loop (i + 1) t l
    in
    loop 0 t all

  let of_int i = List.nth_exn all i

  let count = List.length all

  let initial = to_int Whitespace
  let () = assert (initial = 0) (* This is assumed in parser_automaton_internal.ml *)
  let () = assert (to_int Error = 1) (* This is assumed in parser_automaton_internal.ml *)

  let old_parser_approx_cont_state = function
    | Whitespace -> "Parsing_toplevel_whitespace"
    | After_cr -> "Parsing_nested_whitespace"
    | Unquoted_string _
    | Quoted_string _ -> "Parsing_atom"
    | After_hash -> "Parsing_atom"
    | Block_comment _ -> "Parsing_block_comment"
    | Line_comment -> "Parsing_toplevel_whitespace"
    (* This cannot happen with the old parser so the result is a dummy value *)
    | Error -> "Parsing_toplevel_whitespace"
end

module Error = struct
  (* Subset of the [Parser_automaton_internal.Public.Error.Reason.t] type *)
  type t =
    | Unexpected_char_parsing_hex_escape
    | Unexpected_char_parsing_dec_escape
    | Unterminated_quoted_string
    | Unterminated_block_comment
    | Comment_token_in_unquoted_atom
    | Unexpected_character_after_cr
    | Automaton_in_error_state
  [@@deriving compare, sexp_of, hash, variants]

  let to_string = Variants.to_name
end

(* Action associated to transitions. Actions correspond to the similarly named functions
   in ../parser_automaton_internal.mli. *)
module Action = struct

  type t =
    | Nop
    | Opening
    | Closing
    | Add_atom_char
    | Add_quoted_atom_char
    | Add_first_char
    | Add_escaped
    | Add_hex_escape_char
    | Add_dec_escape_char
    | Add_last_hex_escape_char
    | Add_last_dec_escape_char
    | Add_token_char
    | Comment_add_last_dec_escape_char
    | Push_quoted_atom
    | Start_quoted_string
    | Start_block_comment
    | Start_sexp_comment
    | Start_line_comment
  [@@deriving compare, sexp_of, hash, variants]

  let to_runtime_function = function
    | Nop -> None
    | t   -> Some (String.uncapitalize (Variants.to_name t))
end

(* Action associated to epsilon transitions, i.e. transitions that do not consume a
   character.

   Having epsilon actions makes the definition of the automaton much simpler. *)
module Epsilon_action = struct
  type t =
    | Nop
    | Push_atom
    | Add_first_char_hash
    | Add_escaped_cr
    | End_line_comment
  [@@deriving compare, sexp_of, hash, variants]

  let to_runtime_function = function
    | Nop                 -> None
    | End_line_comment    -> Some "end_line_comment"
    | t                   -> Some ("eps_" ^ String.uncapitalize (Variants.to_name t))
end

module Transition = struct
  type t =
    | T of Action.t         * State.t
    | E of Epsilon_action.t * State.t
    | Error of Error.t
    | End_block_comment (* can't be a normal transition, as the new state isn't known
                           statically *)
  [@@deriving compare]
end

module Final_transition = struct
  type t =
    | Eoi_check
    | E of Epsilon_action.t * State.t
    | Error of Error.t
end

module type Automaton = sig
  val transition     : State.t * char -> Transition.t
  val transition_eoi : State.t -> Final_transition.t
end

(* Definition of the automaton, compiled later to a transition table. *)
module Automaton : Automaton = struct
  module Quoted_string_transition = struct
    type t =
      | T of Action.t         * State.Quoted_string.t
      | E of Epsilon_action.t * State.Quoted_string.t
      | Error of Error.t
      | End_of_quoted_string
  end

  type context = In_block_comment | In_atom

  let quoted_string_transition
    : context -> State.Quoted_string.t * char -> Quoted_string_transition.t
    = fun context x ->
      (* Distinguising atom and block comments is to optimize block comments. But
         we musn't optimize the exception on things like \321. *)
      let if_atom then_ else_ : Action.t =
        match context with
        | In_atom    -> then_
        | In_block_comment -> else_
      in
      let if_atom_eps then_ else_ : Epsilon_action.t =
        match context with
        | In_atom    -> then_
        | In_block_comment -> else_
      in
      match x with
      | Normal, '"'  -> End_of_quoted_string
      | Normal, '\\' -> T (Add_token_char, After_backslash)
      | Normal, _    -> T (if_atom Add_quoted_atom_char Add_token_char, Normal)

      | After_backslash, '\n' ->
        T (Add_token_char, Ignoring_blanks)
      | After_backslash, '\r' ->
        T (Add_token_char, After_backslash_cr)
      | After_backslash, 'x' ->
        T (Add_token_char, After_backslash_x)
      | After_backslash, '0'..'9' ->
        T (Add_dec_escape_char, After_backslash_digit)
      | After_backslash, _ ->
        T (if_atom Add_escaped Add_token_char, Normal)

      | After_backslash_cr, '\n' ->
        T (Add_token_char, Ignoring_blanks)
      | After_backslash_cr, _ ->
        E (if_atom_eps Add_escaped_cr Nop, Normal)

      | After_backslash_x, ('0'..'9' | 'a'..'f' | 'A'..'F') ->
        T (if_atom Add_hex_escape_char Add_token_char , After_backslash_x_hex)
      | After_backslash_x, _ ->
        Error Unexpected_char_parsing_hex_escape

      | After_backslash_x_hex, ('0'..'9' | 'a'..'f' | 'A'..'F') ->
        T (if_atom Add_last_hex_escape_char Add_token_char, Normal)
      | After_backslash_x_hex, _ ->
        Error Unexpected_char_parsing_hex_escape

      | After_backslash_digit, '0'..'9' ->
        T (Add_dec_escape_char, After_backslash_2digits)
      | After_backslash_digit, _ ->
        Error Unexpected_char_parsing_dec_escape

      | After_backslash_2digits, '0'..'9' ->
        T (if_atom Add_last_dec_escape_char Comment_add_last_dec_escape_char, Normal)
      | After_backslash_2digits, _ ->
        Error Unexpected_char_parsing_dec_escape

      | Ignoring_blanks, (' '|'\t') ->
        T (Add_token_char, Ignoring_blanks)
      | Ignoring_blanks, _ ->
        E (Nop, Normal)

  module Block_comment_transition = struct
    type t =
      | T of Action.t         * State.Block_comment.t
      | E of Epsilon_action.t * State.Block_comment.t
      | Error of Error.t
      | End_comment
  end

  let block_comment_transition
    : State.Block_comment.t * char -> Block_comment_transition.t
    = function
      | Quoted_string state, c -> (
          match quoted_string_transition In_block_comment (state, c) with
          | End_of_quoted_string -> T (Add_token_char, Normal)
          | T (action, state) -> T (action, Quoted_string state)
          | E (action, state) -> E (action, Quoted_string state)
          | Error error -> Error error
        )
      | After_hash, '|' -> T (Start_block_comment, Normal)
      | After_pipe, '#'  -> End_comment
      | _, '"' -> T (Add_token_char, Quoted_string Normal)
      | _, '|' -> T (Add_token_char, After_pipe)
      | _, '#' -> T (Add_token_char, After_hash)
      | _, _   -> T (Add_token_char, Normal)

  let transition : State.t * char -> Transition.t = function
    | Whitespace, '(' -> T (Opening, Whitespace)
    | Whitespace, ')' -> T (Closing, Whitespace)
    | Whitespace, '\r' -> T (Nop, After_cr)
    | Whitespace, (' ' | '\t' | '\012' | '\n') -> T (Nop, Whitespace)
    | Whitespace, ';' -> T (Start_line_comment, Line_comment)
    | Whitespace, '"' -> T (Start_quoted_string, Quoted_string (Normal))
    | Whitespace, '#' -> T (Nop, After_hash)
    | Whitespace, '|' -> T (Add_first_char, Unquoted_string After_pipe)
    | Whitespace, _   -> T (Add_first_char, Unquoted_string Normal)

    | After_cr, '\n' -> T (Nop, Whitespace)
    | After_cr, _    -> Error Unexpected_character_after_cr

    | Unquoted_string _, (';'|'('|')'|'"'|' '|'\t'|'\012'|'\r'|'\n') ->
      E (Push_atom, Whitespace)
    | Unquoted_string After_hash, '|'
    | Unquoted_string After_pipe, '#' -> Error Comment_token_in_unquoted_atom
    | Unquoted_string _, '#' -> T (Add_atom_char, Unquoted_string After_hash)
    | Unquoted_string _, '|' -> T (Add_atom_char, Unquoted_string After_pipe)
    | Unquoted_string _, _   -> T (Add_atom_char, Unquoted_string Normal)

    | Line_comment, ('\r' | '\n') -> E (End_line_comment, Whitespace)
    | Line_comment, _ -> T (Add_token_char, Line_comment)

    | After_hash, ';' -> T (Start_sexp_comment, Whitespace)
    | After_hash, '|' -> T (Start_block_comment, Block_comment Normal)
    | After_hash, _   -> E (Add_first_char_hash, Unquoted_string Normal)

    | Quoted_string state, c -> (
        match quoted_string_transition In_atom (state, c) with
        | End_of_quoted_string -> T (Push_quoted_atom, Whitespace)
        | T (action, state) -> T (action, Quoted_string state)
        | E (action, state) -> E (action, Quoted_string state)
        | Error error -> Error error
      )

    | Block_comment state, c -> (
        match block_comment_transition (state, c) with
        | T (action, state) -> T (action, Block_comment state)
        | E (action, state) -> E (action, Block_comment state)
        | End_comment -> End_block_comment
        | Error error -> Error error
      )

    | Error, _ -> Error Automaton_in_error_state

  let transition_eoi : State.t -> Final_transition.t = function
    | Whitespace        -> Eoi_check
    | After_cr          -> Error Unexpected_character_after_cr
    | Unquoted_string _ -> E (Push_atom,        Whitespace)
    | Line_comment      -> E (End_line_comment, Whitespace)
    | After_hash        -> E (Add_first_char_hash, Unquoted_string Normal)
    | Quoted_string _   -> Error Unterminated_quoted_string
    | Block_comment _   -> Error Unterminated_block_comment
    | Error             -> Error Automaton_in_error_state
end

module Table = struct
  type action = Epsilon_action.t list * Action.t [@@deriving compare, sexp_of, hash]
  type goto_state = State of int | End_block_comment [@@deriving compare, sexp_of, hash]
  type advance = Advance | Advance_eol [@@deriving compare, sexp_of, hash]
  type transition = { action : action; goto : goto_state; advance : advance }
  [@@deriving compare, sexp_of, hash]
  type 'a or_error =
    | Ok of 'a
    | Error of Error.t
  [@@deriving compare, sexp_of, hash]

  type t =
    { transitions     : transition or_error array
    ; transitions_eoi : Epsilon_action.t list or_error array
    }

  let advance = function
    | '\n' -> Advance_eol
    | _    -> Advance

  let compile (module A : Automaton) =
    let rec squash acc state c =
      match A.transition (state, c) with
      | T (action, state) -> Ok { action  = (List.rev acc, action)
                                ; goto    = State (State.to_int state)
                                ; advance = advance c
                                }
      | E (action, state) -> squash (action :: acc) state c
      | Error error       -> Error error
      | End_block_comment -> Ok { action  = (List.rev acc, Nop)
                                ; goto    = End_block_comment
                                ; advance = advance c
                                }
    in
    let rec squash_eoi acc state =
      match A.transition_eoi state with
      | Eoi_check             -> Ok (List.rev acc)
      | E (eps_action, state) -> squash_eoi (eps_action :: acc) state
      | Error error           -> Error error
    in
    let transitions =
      Array.create ~len:(State.count * 256)
        (Ok { action  = ([], Action.Nop)
            ; goto    = State 0
            ; advance = Advance
            })
    in
    let transitions_eoi = Array.create (Ok []) ~len:State.count in
    for s = 0 to State.count - 1 do
      let state = State.of_int s in
      for c = 0 to 255 do
        transitions.(s * 256 + c) <- squash [] state (Char.of_int_exn c);
      done;
      transitions_eoi.(s) <- squash_eoi [] state
    done;
    { transitions; transitions_eoi }
end

let table = Table.compile (module Automaton)
