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

let print ppf { start; stop } =
  let start_c = start.pos_cnum - start.pos_bol in
  let stop_c  = stop.pos_cnum  - start.pos_bol in
  Format.fprintf ppf
    "@{<loc>File \"%s\", line %d, characters %d-%d:@}@\n"
    start.pos_fname start.pos_lnum start_c stop_c

let warn t fmt =
  Errors.kerrf ~f:print_to_console
    ("%a@{<warning>Warning@}: " ^^ fmt ^^ "@.") print t

let to_file_colon_line t =
  sprintf "%s:%d" t.start.pos_fname t.start.pos_lnum

let pp_file_colon_line ppf t =
  Format.pp_print_string ppf (to_file_colon_line t)
