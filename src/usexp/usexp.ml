module UnlabeledBytes = Bytes
open StdLabels

module Bytes = struct
  include StdLabels.Bytes

  (* [blit_string] was forgotten from the labeled version in OCaml
     4.02—4.04. *)
  let blit_string ~src ~src_pos ~dst ~dst_pos ~len =
    UnlabeledBytes.blit_string src src_pos dst dst_pos len
end

module A = Parser_automaton_internal

module Atom = struct
  type t = string

  let escaped_length s =
    let n = ref 0 in
    for i = 0 to String.length s - 1 do
      n := !n +
           (match String.unsafe_get s i with
            | '\"' | '\\' | '\n' | '\t' | '\r' | '\b' -> 2
            | ' ' .. '~' -> 1
            | _ -> 4)
    done;
    !n

  let must_escape s =
    let len = String.length s in
    len = 0 || escaped_length s > len

  let escaped_internal s ~with_double_quotes ~always_quote =
    let n = escaped_length s in
    if n > 0 && n = String.length s then
      if always_quote then begin
          let s' = Bytes.create (n + 2) in
          Bytes.unsafe_set s' 0 '"';
          Bytes.blit_string ~src:s ~src_pos:0 ~dst:s' ~dst_pos:1 ~len:n;
          Bytes.unsafe_set s' (n + 1) '"';
          Bytes.unsafe_to_string s'
        end
      else s
    else begin
      let s' = Bytes.create (n + if with_double_quotes then 2 else 0) in
      let n = ref 0 in
      if with_double_quotes then begin
        Bytes.unsafe_set s' 0 '"';
        n := 1
      end;
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
      done;
      if with_double_quotes then Bytes.unsafe_set s' !n '"';
      Bytes.unsafe_to_string s'
    end

  let escaped s =
    escaped_internal s ~with_double_quotes:false ~always_quote:false
  let serialize s =
    escaped_internal s ~with_double_quotes:true ~always_quote:false
  let quote s =
    escaped_internal s ~with_double_quotes:true ~always_quote:true
end

type t =
  | Atom of string
  | Quoted_string of string
  | List of t list

type sexp = t

let rec to_string = function
  | Atom s -> Atom.serialize s
  | Quoted_string s -> Atom.quote s
  | List l -> Printf.sprintf "(%s)" (List.map l ~f:to_string |> String.concat ~sep:" ")

let rec pp ppf = function
  | Atom s ->
    Format.pp_print_string ppf (Atom.serialize s)
  | Quoted_string s ->
    Format.pp_print_string ppf (Atom.quote s)
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

let pp_print_atom ppf ~serialize s =
  if String.contains s '\n' then begin
    match split_string s ~on:'\n' with
    | [] -> Format.pp_print_string ppf (serialize s)
    | first :: rest ->
       Format.fprintf ppf "@[<hv 1>\"@{<atom>%s" (Atom.escaped first);
       List.iter rest ~f:(fun s ->
           Format.fprintf ppf "@,\\n%s" (Atom.escaped s));
       Format.fprintf ppf "@}\"@]"
  end else
    Format.pp_print_string ppf (serialize s)

let rec pp_split_strings ppf = function
  | Atom s ->
    if Atom.must_escape s then
      pp_print_atom ppf s ~serialize:Atom.serialize
    else
      Format.pp_print_string ppf s
  | Quoted_string s ->
     pp_print_atom ppf s ~serialize:Atom.quote
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

module Loc = Sexp_ast.Loc

module Ast = struct
  type t = Sexp_ast.t =
    | Atom of Loc.t * string
    | Quoted_string of Loc.t * string
    | List of Loc.t * t list

  let loc (Atom (loc, _) | Quoted_string (loc, _) | List (loc, _)) = loc

  let rec remove_locs : t -> sexp = function
    | Atom (_, s) -> Atom s
    | Quoted_string (_, s) -> Quoted_string s
    | List (_, l) -> List (List.map l ~f:remove_locs)

  module Token = struct
    type t =
      | Atom   of Loc.t * string
      | String of Loc.t * string
      | Lparen of Loc.t
      | Rparen of Loc.t
  end

  let tokenize =
    let rec loop acc t =
      match t with
      | Atom (loc, s) -> Token.Atom (loc, s) :: acc
      | Quoted_string (loc, s) -> Token.String (loc, s) :: acc
      | List (loc, l) ->
        let shift (pos : Lexing.position) delta =
          { pos with pos_cnum = pos.pos_cnum + delta }
        in
        let l_loc = { loc with stop  = shift loc.start  1  } in
        let r_loc = { loc with start = shift loc.stop (-1) } in
        let acc = Token.Lparen l_loc :: acc in
        let acc = List.fold_left l ~init:acc ~f:loop in
        let acc = Token.Rparen r_loc :: acc in
        acc
    in
    fun t -> loop [] t |> List.rev
end

let rec add_loc t ~loc : Ast.t =
  match t with
  | Atom s -> Atom (loc, s)
  | Quoted_string s -> Quoted_string (loc, s)
  | List l -> List (loc, List.map l ~f:(add_loc ~loc))

module Parser = struct
  module Error = A.Error
  exception Error = A.Parse_error

  module Mode = struct
    type 'a t = 'a A.mode =
      | Single : Ast.t t
      | Many   : Ast.t list t
  end

  module Stack = struct
    type t = A.stack
    let empty = A.empty_stack
  end

  type 'a t = 'a A.state
  let create ~fname ~mode = A.new_state ~fname mode

  let feed : type a. a A.action = fun state char stack ->
    let idx = (A.automaton_state state lsl 8) lor (Char.code char) in
    (* We need an Obj.magic as the type of the array can't be generalized.
       This problem will go away when we get immutable arrays. *)
    (Obj.magic (Table.transitions.(idx) : Obj.t A.action) : a A.action) state char stack
  [@@inline always]

  let feed_eoi : type a. a t -> Stack.t -> a = fun state stack ->
    let stack =
      (Obj.magic (Table.transitions_eoi.(A.automaton_state state)
                  : Obj.t A.epsilon_action)
       : a A.epsilon_action) state stack
    in
    A.set_error_state state;
    match A.mode state with
    | Mode.Single -> A.sexp_of_stack stack
    | Mode.Many   -> A.sexps_of_stack stack

  let rec feed_substring_unsafe str state stack i stop =
    if i < stop then
      let c = String.unsafe_get str i in
      let stack = feed state c stack in
      feed_substring_unsafe str state stack (i + 1) stop
    else
      stack

  let rec feed_subbytes_unsafe str state stack i stop =
    if i < stop then
      let c = Bytes.unsafe_get str i in
      let stack = feed state c stack in
      feed_subbytes_unsafe str state stack (i + 1) stop
    else
      stack

  let feed_substring state str ~pos ~len stack =
    let str_len = String.length str in
    if pos < 0 || len < 0 || pos > str_len - len then
      invalid_arg "Jbuilder_sexp.feed_substring";
    feed_substring_unsafe str state stack pos (pos + len)

  let feed_subbytes state str ~pos ~len stack =
    let str_len = Bytes.length str in
    if pos < 0 || len < 0 || pos > str_len - len then
      invalid_arg "Jbuilder_sexp.feed_subbytes";
    feed_subbytes_unsafe str state stack pos (pos + len)

  let feed_string state str stack =
    feed_substring_unsafe str state stack 0 (String.length str)

  let feed_bytes state str stack =
    feed_subbytes_unsafe str state stack 0 (Bytes.length str)
end

let parse_string ~fname ~mode str =
  let p = Parser.create ~fname ~mode in
  let stack = Parser.feed_string p str Parser.Stack.empty in
  Parser.feed_eoi p stack
