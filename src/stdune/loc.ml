include Loc0

let none_pos p : Lexing.position =
  { pos_fname = p
  ; pos_lnum  = 1
  ; pos_cnum  = 0
  ; pos_bol   = 0
  }

let in_file p =
  let pos = none_pos (Path.to_string p) in
  { start = pos
  ; stop = pos
  }

let in_dir = in_file

let none =
  let pos = none_pos "<none>" in
  { start = pos
  ; stop = pos
  }

let drop_position (t : t) =
  let pos = none_pos t.start.pos_fname in
  { start = pos
  ; stop = pos
  }

let of_lexbuf lexbuf : t =
  { start = Lexing.lexeme_start_p lexbuf
  ; stop  = Lexing.lexeme_end_p   lexbuf
  }

let sexp_of_position_no_file (p : Lexing.position) =
  let open Sexp.Encoder in
  record
    [ "pos_lnum", int p.pos_lnum
    ; "pos_bol", int p.pos_bol
    ; "pos_cnum", int p.pos_cnum
    ]

let to_sexp t =
  let open Sexp.Encoder in
  record (* TODO handle when pos_fname differs *)
    [ "pos_fname", string t.start.pos_fname
    ; "start", sexp_of_position_no_file t.start
    ; "stop", sexp_of_position_no_file t.stop
    ]

let equal_position
      { Lexing.pos_fname = f_a; pos_lnum = l_a
      ; pos_bol = b_a; pos_cnum = c_a }
      { Lexing.pos_fname = f_b; pos_lnum = l_b
      ; pos_bol = b_b; pos_cnum = c_b }
      =
      f_a = f_b
      && l_a = l_b
      && b_a = b_b
      && c_a = c_b

let equal
      { start = start_a ; stop = stop_a }
      { start = start_b ; stop = stop_b }
  =
  equal_position start_a start_b
  && equal_position stop_a stop_b

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

let to_file_colon_line t =
  Printf.sprintf "%s:%d" t.start.pos_fname t.start.pos_lnum

let pp_file_colon_line ppf t =
  Format.pp_print_string ppf (to_file_colon_line t)

let pp_line padding_width pp (lnum, l) =
  Format.fprintf pp "%*s | %s\n" padding_width lnum l

let pp_file_excerpt ~context_lines ~max_lines_to_print_in_full
      pp { start; stop } =
  let start_c = start.pos_cnum - start.pos_bol in
  let stop_c  = stop.pos_cnum  - start.pos_bol in
  let num_lines = stop.pos_lnum - start.pos_lnum in
  let whole_file = start_c = 0 && stop_c = 0 in
  if not whole_file then
    let path = Path.of_string start.pos_fname in
    if Path.exists path then
      let line_num = start.pos_lnum in
      let line_num_str = string_of_int line_num in
      let padding_width = String.length line_num_str in
      let line = Io.file_line path line_num in
      if stop_c <= String.length line then
        let len = stop_c - start_c in
        Format.fprintf pp "%a%*s\n"
          (pp_line padding_width) (line_num_str, line)
          (stop_c + padding_width + 3)
          (String.make len '^')
      else
        let get_padding lines =
          let (lnum, _) = Option.value_exn (List.last lines) in
          String.length lnum
        in
        let print_ellipsis padding_width =
          (* We add 2 to the width of max line to account for the extra space
             and the `|` character at the end of a line number *)
          let line = String.make (padding_width + 2) '.' in
          Format.fprintf pp "%s\n" line
        in
        let print_lines lines padding_width =
          List.iter ~f:(fun (lnum, l) ->
            pp_line padding_width pp (lnum, l)) lines;
        in
        if num_lines <= max_lines_to_print_in_full then
          let lines =
            Io.file_lines path ~start:start.pos_lnum ~stop:stop.pos_lnum in
          print_lines lines (get_padding lines)
        else
          (* We need to send the padding width from the last four lines so the
             two blocks of lines align if they have different number of digits
             in their line numbers *)
          let first_shown_lines =
            Io.file_lines path ~start:(start.pos_lnum)
              ~stop:(start.pos_lnum + context_lines) in
          let last_shown_lines =
            Io.file_lines path ~start:(stop.pos_lnum - context_lines)
              ~stop:(stop.pos_lnum) in
          let padding_width = get_padding last_shown_lines in
          (print_lines first_shown_lines padding_width;
           print_ellipsis padding_width;
           print_lines last_shown_lines padding_width)
