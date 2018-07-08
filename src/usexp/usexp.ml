open Import

module Loc = Loc
module Atom = Atom
module Template = Template

type syntax = Atom.syntax = Jbuild | Dune

include Sexp

let atom s = Atom (Atom.of_string s)

let unsafe_atom_of_string s = atom s

let rec to_string t ~syntax =
  match t with
  | Atom a -> Atom.print a syntax
  | Quoted_string s -> Escape.quoted s ~syntax
  | List l ->
    Printf.sprintf "(%s)" (List.map l ~f:(to_string ~syntax)
                           |> String.concat ~sep:" ")
  | Template t -> Template.to_string t ~syntax

let rec pp syntax ppf = function
  | Atom s ->
    Format.pp_print_string ppf (Atom.print s syntax)
  | Quoted_string s ->
    Format.pp_print_string ppf (Escape.quoted ~syntax s)
  | List [] ->
    Format.pp_print_string ppf "()"
  | List (first :: rest) ->
    Format.pp_open_box ppf 1;
    Format.pp_print_string ppf "(";
    Format.pp_open_hvbox ppf 0;
    pp syntax ppf first;
    List.iter rest ~f:(fun sexp ->
      Format.pp_print_space ppf ();
      pp syntax ppf sexp);
    Format.pp_close_box ppf ();
    Format.pp_print_string ppf ")";
    Format.pp_close_box ppf ()
  | Template t -> Template.pp syntax ppf t

let pp_quoted =
  let rec loop = function
    | Atom (A s) as t ->
      if Atom.is_valid_dune s then
        t
      else
        Quoted_string s
    | List xs -> List (List.map ~f:loop xs)
    | (Quoted_string _ | Template _) as t -> t
  in
  fun ppf t -> pp Dune ppf (loop t)

let pp_print_quoted_string ppf s =
  let syntax = Dune in
  if String.contains s '\n' then begin
    match String.split_on_char s ~on:'\n' with
    | [] -> Format.pp_print_string ppf (Escape.quoted ~syntax s)
    | first :: rest ->
       Format.fprintf ppf "@[<hv 1>\"@{<atom>%s"
         (Escape.escaped ~syntax first);
       List.iter rest ~f:(fun s ->
           Format.fprintf ppf "@,\\n%s" (Escape.escaped ~syntax s));
       Format.fprintf ppf "@}\"@]"
  end else
    Format.pp_print_string ppf (Escape.quoted ~syntax s)

let rec pp_split_strings ppf = function
  | Atom s -> Format.pp_print_string ppf (Atom.print s Atom.Dune)
  | Quoted_string s -> pp_print_quoted_string ppf s
  | List [] ->
    Format.pp_print_string ppf "()"
  | List (first :: rest) ->
    Format.pp_open_box ppf 1;
    Format.pp_print_string ppf "(";
    Format.pp_open_hvbox ppf 0;
    pp_split_strings ppf first;
    List.iter rest ~f:(fun sexp ->
      Format.pp_print_space ppf ();
      pp_split_strings ppf sexp);
    Format.pp_close_box ppf ();
    Format.pp_print_string ppf ")";
    Format.pp_close_box ppf ()
  | Template t -> Template.pp_split_strings ppf t

type formatter_state =
  | In_atom
  | In_makefile_action
  | In_makefile_stuff

let prepare_formatter ppf =
  let state = ref [] in
  Format.pp_set_mark_tags ppf true;
  let ofuncs = Format.pp_get_formatter_out_functions ppf () in
  let tfuncs = Format.pp_get_formatter_tag_functions ppf () in
  Format.pp_set_formatter_tag_functions ppf
    { tfuncs with
      mark_open_tag  = (function
        | "atom" -> state := In_atom :: !state; ""
        | "makefile-action" -> state := In_makefile_action :: !state; ""
        | "makefile-stuff" -> state := In_makefile_stuff :: !state; ""
        | s -> tfuncs.mark_open_tag s)
    ; mark_close_tag = (function
        | "atom" | "makefile-action" | "makefile-stuff" -> state := List.tl !state; ""
        | s -> tfuncs.mark_close_tag s)
    };
  Format.pp_set_formatter_out_functions ppf
    { ofuncs with
      out_newline = (fun () ->
        match !state with
        | [In_atom; In_makefile_action] ->
          ofuncs.out_string "\\\n\t" 0 3
        | [In_atom] ->
          ofuncs.out_string "\\\n" 0 2
        | [In_makefile_action] ->
          ofuncs.out_string " \\\n\t" 0 4
        | [In_makefile_stuff] ->
          ofuncs.out_string " \\\n" 0 3
        | [] ->
          ofuncs.out_string "\n" 0 1
        | _ -> assert false)
    ; out_spaces = (fun n ->
        ofuncs.out_spaces
          (match !state with
           | In_atom :: _ -> max 0 (n - 2)
           | _ -> n))
    }

module Ast = struct
  type t =
    | Atom of Loc.t * Atom.t
    | Quoted_string of Loc.t * string
    | Template of Template.t
    | List of Loc.t * t list

  let atom_or_quoted_string loc s =
    match Sexp.atom_or_quoted_string s with
    | Atom a -> Atom (loc, a)
    | Quoted_string s -> Quoted_string (loc, s)
    | Template _
    | List _ -> assert false

  let loc (Atom (loc, _) | Quoted_string (loc, _) | List (loc, _)
          | Template { loc ; _ }) = loc

  let rec remove_locs t : Sexp.t =
    match t with
    | Template t -> Template (Template.remove_locs t)
    | Atom (_, s) -> Atom s
    | Quoted_string (_, s) -> Quoted_string s
    | List (_, l) -> List (List.map l ~f:remove_locs)
end

let rec add_loc t ~loc : Ast.t =
  match t with
  | Atom s -> Atom (loc, s)
  | Quoted_string s -> Quoted_string (loc, s)
  | List l -> List (loc, List.map l ~f:(add_loc ~loc))
  | Template t -> Template { t with loc }

module Parse_error = struct
  include Lexer.Error

  let loc t : Loc.t = { start = t.start; stop = t.stop }
  let message t = t.message
end
exception Parse_error = Lexer.Error

module Lexer = Lexer

module Parser = struct
  let error (loc : Loc.t) message =
    raise (Parse_error
             { start = loc.start
             ; stop  = loc.stop
             ; message
             })

  module Mode = struct
    type 'a t =
      | Single      : Ast.t t
      | Many        : Ast.t list t
      | Many_as_one : Ast.t t

    let make_result : type a. a t -> Lexing.lexbuf -> Ast.t list -> a
      = fun t lexbuf sexps ->
        match t with
        | Single -> begin
          match sexps with
          | [sexp] -> sexp
          | [] -> error (Loc.of_lexbuf lexbuf) "no s-expression found in input"
          | _ :: sexp :: _ ->
            error (Ast.loc sexp) "too many s-expressions found in input"
        end
        | Many -> sexps
        | Many_as_one ->
          match sexps with
          | [] -> List (Loc.in_file lexbuf.lex_curr_p.pos_fname, [])
          | x :: l ->
            let last = List.fold_left l ~init:x ~f:(fun _ x -> x) in
            let loc = { (Ast.loc x) with stop = (Ast.loc last).stop } in
            List (loc, x :: l)
  end

  let rec loop depth lexer lexbuf acc =
    match (lexer lexbuf : Lexer.Token.t) with
    | Atom a ->
      let loc = Loc.of_lexbuf lexbuf in
      loop depth lexer lexbuf (Ast.Atom (loc, a) :: acc)
    | Quoted_string s ->
      let loc = Loc.of_lexbuf lexbuf in
      loop depth lexer lexbuf (Quoted_string (loc, s) :: acc)
    | Template t ->
      let loc = Loc.of_lexbuf lexbuf in
      loop depth lexer lexbuf (Template { t with loc } :: acc)
    | Lparen ->
      let start = Lexing.lexeme_start_p lexbuf in
      let sexps = loop (depth + 1) lexer lexbuf [] in
      let stop = Lexing.lexeme_end_p lexbuf in
      loop depth lexer lexbuf (List ({ start; stop }, sexps) :: acc)
    | Rparen ->
      if depth = 0 then
        error (Loc.of_lexbuf lexbuf)
          "right parenthesis without matching left parenthesis";
      List.rev acc
    | Sexp_comment ->
      let sexps =
        let loc = Loc.of_lexbuf lexbuf in
        match loop depth lexer lexbuf [] with
        | _ :: sexps -> sexps
        | [] -> error loc "s-expression missing after #;"
      in
      List.rev_append acc sexps
    | Eof ->
      if depth > 0 then
        error (Loc.of_lexbuf lexbuf)
          "unclosed parenthesis at end of input";
      List.rev acc

  let parse ~mode ?(lexer=Lexer.token) lexbuf =
    loop 0 lexer lexbuf []
    |> Mode.make_result mode lexbuf
end

let parse_string ~fname ~mode ?lexer str =
  let lb = Lexing.from_string str in
  lb.lex_curr_p <-
    { pos_fname = fname
    ; pos_lnum  = 1
    ; pos_bol   = 0
    ; pos_cnum  = 0
    };
  Parser.parse ~mode ?lexer lb
