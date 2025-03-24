include Loc0
module O = Comparable.Make (Loc0)
include O

let in_file p = Lexbuf.Loc.in_file ~fname:(Path.to_string p) |> of_lexbuf_loc
let in_dir = in_file

let drop_position (t : t) =
  let pos = Lexbuf.Position.in_file ~fname:(start t).pos_fname in
  create ~start:pos ~stop:pos
;;

let of_lexbuf lexbuf : t =
  create ~start:(Lexing.lexeme_start_p lexbuf) ~stop:(Lexing.lexeme_end_p lexbuf)
;;

let of_pos pos = Lexbuf.Loc.of_pos pos |> of_lexbuf_loc

let to_file_colon_line t =
  let start = start t in
  Printf.sprintf "%s:%d" start.pos_fname start.pos_lnum
;;

let to_dyn_hum t : Dyn.t = String (to_file_colon_line t)
let pp_file_colon_line t = Pp.verbatim (to_file_colon_line t)

let pp_left_pad n s =
  let needed_spaces = n - String.length s in
  Pp.verbatim (if needed_spaces > 0 then String.make needed_spaces ' ' ^ s else s)
;;

let pp_line padding_width (lnum, l) =
  let open Pp.O in
  pp_left_pad padding_width lnum ++ Pp.verbatim " | " ++ Pp.verbatim l ++ Pp.newline
;;

type tag = Loc

let pp_file_excerpt ~context_lines ~max_lines_to_print_in_full loc : tag Pp.t =
  let start = start loc in
  let stop = stop loc in
  let start_c = start.pos_cnum - start.pos_bol in
  let stop_c = stop.pos_cnum - start.pos_bol in
  let file = start.pos_fname in
  let pp_file_excerpt () =
    let open Result.O in
    match if start.pos_lnum <> stop.pos_lnum then `Multiline else `Singleline with
    | `Singleline ->
      let line_num = start.pos_lnum in
      let line_num_str = string_of_int line_num in
      let padding_width = String.length line_num_str in
      let* line = Result.try_with (fun () -> Io.String_path.file_line file line_num) in
      let len = stop_c - start_c in
      let open Pp.O in
      Ok
        (pp_line padding_width (line_num_str, line)
         ++ pp_left_pad (stop_c + padding_width + 3) (String.make len '^')
         ++ Pp.newline)
    | `Multiline ->
      let get_padding lines =
        let lnum, _ = Option.value_exn (List.last lines) in
        String.length lnum
      in
      let print_ellipsis padding_width =
        (* We add 2 to the width of max line to account for the extra space and
           the `|` character at the end of a line number *)
        let line = String.make (padding_width + 2) '.' in
        let open Pp.O in
        Pp.verbatim line ++ Pp.newline
      in
      let print_lines lines padding_width =
        Pp.concat_map lines ~f:(pp_line padding_width)
      in
      let file_lines ~start ~stop =
        Result.try_with (fun () -> Io.String_path.file_lines file ~start ~stop)
      in
      let num_lines = stop.pos_lnum - start.pos_lnum in
      if num_lines <= max_lines_to_print_in_full
      then
        let+ lines = file_lines ~start:start.pos_lnum ~stop:stop.pos_lnum in
        print_lines lines (get_padding lines)
      else
        (* We need to send the padding width from the last four lines so the two
           blocks of lines align if they have different number of digits in
           their line numbers *)
        let* first_shown_lines =
          file_lines ~start:start.pos_lnum ~stop:(start.pos_lnum + context_lines)
        in
        let+ last_shown_lines =
          file_lines ~start:(stop.pos_lnum - context_lines) ~stop:stop.pos_lnum
        in
        let padding_width = get_padding last_shown_lines in
        let open Pp.O in
        print_lines first_shown_lines padding_width
        ++ print_ellipsis padding_width
        ++ print_lines last_shown_lines padding_width
  in
  let whole_file = start_c = 0 && stop_c = 0 in
  if whole_file
  then Pp.nop
  else (
    match
      let open Result.O in
      let* exists = Result.try_with (fun () -> Sys.file_exists start.pos_fname) in
      if exists then pp_file_excerpt () else Result.Ok Pp.nop
    with
    | Ok pp -> pp
    | Error exn ->
      let backtrace = Printexc.get_backtrace () in
      Format.eprintf
        "Raised when trying to print location contents of %s@.%a@."
        file
        (Exn.pp_uncaught ~backtrace)
        exn;
      Pp.nop)
;;

let pp loc =
  let start = start loc in
  let stop = stop loc in
  let start_c = start.pos_cnum - start.pos_bol in
  let stop_c = stop.pos_cnum - start.pos_bol in
  let lnum =
    if start.pos_lnum = stop.pos_lnum
    then Printf.sprintf "line %d" start.pos_lnum
    else Printf.sprintf "lines %d-%d" start.pos_lnum stop.pos_lnum
  in
  let open Pp.O in
  Pp.tag
    Loc
    (Pp.verbatim
       (Printf.sprintf
          "File \"%s\", %s, characters %d-%d:"
          start.pos_fname
          lnum
          start_c
          stop_c))
  ++ Pp.newline
  ++ pp_file_excerpt ~context_lines:2 ~max_lines_to_print_in_full:10 loc
;;

let on_same_line loc1 loc2 =
  let start1 = start loc1 in
  let start2 = start loc2 in
  let same_file = String.equal start1.pos_fname start2.pos_fname in
  let same_line = Int.equal start1.pos_lnum start2.pos_lnum in
  same_file && same_line
;;

let span (a : t) (b : t) =
  let earliest_start =
    if (start a).pos_cnum < (start b).pos_cnum then start a else start b
  in
  let latest_stop = if (stop a).pos_cnum > (stop b).pos_cnum then stop a else stop b in
  create ~start:earliest_start ~stop:latest_stop
;;

let rec render ppf pp =
  Pp.to_fmt_with_tags ppf pp ~tag_handler:(fun ppf Loc pp ->
    Format.fprintf ppf "@{<loc>%a@}" render pp)
;;
