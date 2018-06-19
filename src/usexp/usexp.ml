module UnlabeledBytes = Bytes
open StdLabels

module Bytes = struct
  include StdLabels.Bytes

  (* [blit_string] was forgotten from the labeled version in OCaml
     4.02—4.04. *)
  let blit_string ~src ~src_pos ~dst ~dst_pos ~len =
    UnlabeledBytes.blit_string src src_pos dst dst_pos len
end

module Atom = Atom

type t =
  | Atom of Atom.t
  | Quoted_string of string
  | List of t list

type sexp = t

let atom s = Atom (Atom.of_string_exn Dune s)

let unsafe_atom_of_string s = atom s

let atom_or_quoted_string s =
  match Atom.of_string Atom.Dune s with
  | None -> Quoted_string s
  | Some a -> Atom a

let quote_length s =
  let n = ref 0 in
  for i = 0 to String.length s - 1 do
    n := !n + (match String.unsafe_get s i with
               | '\"' | '\\' | '\n' | '\t' | '\r' | '\b' -> 2
               | ' ' .. '~' -> 1
               | _ -> 4)
  done;
  !n

let escape_to s ~dst:s' ~ofs =
  let n = ref ofs in
  for i = 0 to String.length s - 1 do
    begin match String.unsafe_get s i with
    | ('\"' | '\\') as c ->
       Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n c
    | '\n' ->
       Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n 'n'
    | '\t' ->
       Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n 't'
    | '\r' ->
       Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n 'r'
    | '\b' ->
       Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n 'b'
    | (' ' .. '~') as c -> Bytes.unsafe_set s' !n c
    | c ->
       let a = Char.code c in
       Bytes.unsafe_set s' !n '\\';
       incr n;
       Bytes.unsafe_set s' !n (Char.unsafe_chr (48 + a / 100));
       incr n;
       Bytes.unsafe_set s' !n (Char.unsafe_chr (48 + (a / 10) mod 10));
       incr n;
       Bytes.unsafe_set s' !n (Char.unsafe_chr (48 + a mod 10));
    end;
    incr n
  done

(* Escape [s] if needed. *)
let escaped s =
  let n = quote_length s in
  if n = 0 || n > String.length s then
    let s' = Bytes.create n in
    escape_to s ~dst:s' ~ofs:0;
    Bytes.unsafe_to_string s'
  else s

(* Surround [s] with quotes, escaping it if necessary. *)
let quoted s =
  let len = String.length s in
  let n = quote_length s in
  let s' = Bytes.create (n + 2) in
  Bytes.unsafe_set s' 0 '"';
  if len = 0 || n > len then
    escape_to s ~dst:s' ~ofs:1
  else
    Bytes.blit_string ~src:s ~src_pos:0 ~dst:s' ~dst_pos:1 ~len;
  Bytes.unsafe_set s' (n + 1) '"';
  Bytes.unsafe_to_string s'

let rec to_string = function
  | Atom a -> Atom.to_string a Atom.Dune
  | Quoted_string s -> quoted s
  | List l -> Printf.sprintf "(%s)" (List.map l ~f:to_string |> String.concat ~sep:" ")

let rec pp ppf = function
  | Atom s ->
    Format.pp_print_string ppf (Atom.to_string s Atom.Dune)
  | Quoted_string s ->
    Format.pp_print_string ppf (quoted s)
  | List [] ->
    Format.pp_print_string ppf "()"
  | List (first :: rest) ->
    Format.pp_open_box ppf 1;
    Format.pp_print_string ppf "(";
    Format.pp_open_hvbox ppf 0;
    pp ppf first;
    List.iter rest ~f:(fun sexp ->
      Format.pp_print_space ppf ();
      pp ppf sexp);
    Format.pp_close_box ppf ();
    Format.pp_print_string ppf ")";
    Format.pp_close_box ppf ()

let split_string s ~on =
  let rec loop i j =
    if j = String.length s then
      [String.sub s ~pos:i ~len:(j - i)]
    else if s.[j] = on then
      String.sub s ~pos:i ~len:(j - i) :: loop (j + 1) (j + 1)
    else
      loop i (j + 1)
  in
  loop 0 0

let pp_print_quoted_string ppf s =
  if String.contains s '\n' then begin
    match split_string s ~on:'\n' with
    | [] -> Format.pp_print_string ppf (quoted s)
    | first :: rest ->
       Format.fprintf ppf "@[<hv 1>\"@{<atom>%s" (escaped first);
       List.iter rest ~f:(fun s ->
           Format.fprintf ppf "@,\\n%s" (escaped s));
       Format.fprintf ppf "@}\"@]"
  end else
    Format.pp_print_string ppf (quoted s)

let rec pp_split_strings ppf = function
  | Atom s -> Format.pp_print_string ppf (Atom.to_string s Atom.Dune)
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

module Loc = struct
  type t =
    { start : Lexing.position
    ; stop  : Lexing.position
    }

  let in_file fn =
    let pos : Lexing.position =
      { pos_fname = fn
      ; pos_lnum  = 1
      ; pos_cnum  = 0
      ; pos_bol   = 0
      }
    in
    { start = pos
    ; stop = pos
    }
end

module Ast = struct
  type t =
    | Atom of Loc.t * Atom.t
    | Quoted_string of Loc.t * string
    | List of Loc.t * t list

  let atom_or_quoted_string loc s =
    match Atom.of_string Atom.Dune s with
    | None -> Quoted_string (loc, s)
    | Some a -> Atom (loc, a)

  let loc (Atom (loc, _) | Quoted_string (loc, _) | List (loc, _)) = loc

  let rec remove_locs : t -> sexp = function
    | Atom (_, s) -> Atom s
    | Quoted_string (_, s) -> Quoted_string s
    | List (_, l) -> List (List.map l ~f:remove_locs)
end

let rec add_loc t ~loc : Ast.t =
  match t with
  | Atom s -> Atom (loc, s)
  | Quoted_string s -> Quoted_string (loc, s)
  | List l -> List (loc, List.map l ~f:(add_loc ~loc))

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

  let make_loc lexbuf : Loc.t =
    { start = Lexing.lexeme_start_p lexbuf
    ; stop  = Lexing.lexeme_end_p   lexbuf
    }

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
          | [] -> error (make_loc lexbuf) "no s-expression found in input"
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
      let loc = make_loc lexbuf in
      loop depth lexer lexbuf (Ast.Atom (loc, a) :: acc)
    | Quoted_string s ->
      let loc = make_loc lexbuf in
      loop depth lexer lexbuf (Quoted_string (loc, s) :: acc)
    | Lparen ->
      let start = Lexing.lexeme_start_p lexbuf in
      let sexps = loop (depth + 1) lexer lexbuf [] in
      let stop = Lexing.lexeme_end_p lexbuf in
      loop depth lexer lexbuf (List ({ start; stop }, sexps) :: acc)
    | Rparen ->
      if depth = 0 then
        error (make_loc lexbuf)
          "right parenthesis without matching left parenthesis";
      List.rev acc
    | Sexp_comment ->
      let sexps =
        let loc = make_loc lexbuf in
        match loop depth lexer lexbuf [] with
        | _ :: sexps -> sexps
        | [] -> error loc "s-expression missing after #;"
      in
      List.rev_append acc sexps
    | Eof ->
      if depth > 0 then
        error (make_loc lexbuf)
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
