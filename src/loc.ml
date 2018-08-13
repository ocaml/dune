open Import

include Usexp.Loc

(* TODO get rid of all this stuff once this parsing code moves to Usexp and
   there will be no circular dependency *)
let int n = Usexp.Atom (Usexp.Atom.of_int n)
let string = Usexp.atom_or_quoted_string
let record l =
  let open Usexp in
  List (List.map l ~f:(fun (n, v) -> List [Atom(Atom.of_string n); v]))

let sexp_of_position_no_file (p : Lexing.position) =
  record
    [ "pos_lnum", int p.pos_lnum
    ; "pos_bol", int p.pos_bol
    ; "pos_cnum", int p.pos_cnum
    ]

let sexp_of_t t =
  record (* TODO handle when pos_fname differs *)
    [ "pos_fname", string t.start.pos_fname
    ; "start", sexp_of_position_no_file t.start
    ; "stop", sexp_of_position_no_file t.stop
    ]

let of_lexbuf lb =
  { start = Lexing.lexeme_start_p lb
  ; stop  = Lexing.lexeme_end_p   lb
  }

let exnf t fmt =
  Format.pp_open_box err_ppf 0;
  Format.pp_print_as err_ppf 7 ""; (* "Error: " *)
  kerrf (fmt^^ "@]") ~f:(fun s -> Exn.Loc_error (t, s))

let fail t fmt =
  Format.pp_print_as err_ppf 7 ""; (* "Error: " *)
  kerrf fmt ~f:(fun s ->
    raise (Exn.Loc_error (t, s)))

let fail_lex lb fmt =
  fail (of_lexbuf lb) fmt

let fail_opt t fmt =
  match t with
  | None -> die fmt
  | Some t -> fail t fmt

let in_file = Usexp.Loc.in_file

let of_pos (fname, lnum, cnum, enum) =
  let pos : Lexing.position =
    { pos_fname = fname
    ; pos_lnum  = lnum
    ; pos_cnum  = cnum
    ; pos_bol   = 0
    }
  in
  { start = pos
  ; stop  = { pos with pos_cnum = enum }
  }

let file_line path n =
  Io.with_file_in ~binary:false path
    ~f:(fun ic ->
      for _ = 1 to n - 1 do
        ignore (input_line ic)
      done;
      input_line ic
    )

let print ppf loc =
  let { start; stop } = loc in
  let start_c = start.pos_cnum - start.pos_bol in
  let stop_c  = stop.pos_cnum  - start.pos_bol in
  let pp_file_excerpt pp () =
    let whole_file = start_c = 0 && stop_c = 0 in
    if not whole_file then
      let path = Path.of_string start.pos_fname in
      if Path.exists path then
        let line = file_line path start.pos_lnum in
        if stop_c <= String.length line then
          let len = stop_c - start_c in
          Format.fprintf pp "%s\n%*s\n" line
            stop_c
            (String.make len '^')
  in
  Format.fprintf ppf
    "@{<loc>File \"%s\", line %d, characters %d-%d:@}@\n%a"
    start.pos_fname start.pos_lnum start_c stop_c
    pp_file_excerpt ()

let warn t fmt =
  Errors.kerrf ~f:print_to_console
    ("%a@{<warning>Warning@}: " ^^ fmt ^^ "@.") print t

let to_file_colon_line t =
  sprintf "%s:%d" t.start.pos_fname t.start.pos_lnum

let pp_file_colon_line ppf t =
  Format.pp_print_string ppf (to_file_colon_line t)

let equal_position
      { Lexing.pos_fname = f_a; pos_lnum = l_a
      ; pos_bol = b_a; pos_cnum = c_a }
      { Lexing.pos_fname = f_b; pos_lnum = l_b
      ; pos_bol = b_b; pos_cnum = c_b }
      =
      let open Int.Infix in
      String.equal f_a f_b
      && l_a = l_b
      && b_a = b_b
      && c_a = c_b

let equal
      { start = start_a ; stop = stop_a }
      { start = start_b ; stop = stop_b }
  =
  equal_position start_a start_b
  && equal_position stop_a stop_b
