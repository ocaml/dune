open! Stdune
open Import

include Loc

let in_file = Loc.in_file

let file_line path n =
  Io.with_file_in ~binary:false path
    ~f:(fun ic ->
      for _ = 1 to n - 1 do
        ignore (input_line ic)
      done;
      input_line ic
    )

let file_lines path ~start ~stop =
  Io.with_file_in ~binary:true path
    ~f:(fun ic ->
      let rec aux acc lnum =
        if lnum > stop then
          List.rev acc
        else if lnum < start then
          (ignore (input_line ic);
           aux acc (lnum + 1))
        else
          let line = input_line ic in
          aux ((string_of_int lnum, line) :: acc) (lnum + 1)
      in
      aux [] 1
    )

let print ppf loc =
  let { Loc.start; stop } = loc in
  let start_c = start.pos_cnum - start.pos_bol in
  let stop_c  = stop.pos_cnum  - start.pos_bol in
  let num_lines = stop.pos_lnum - start.pos_lnum in
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
        else if num_lines <= 10 then
          let lines = file_lines path ~start:start.pos_lnum ~stop:stop.pos_lnum in
          let last_lnum = Option.map ~f:fst (List.last lines) in
          let padding_width = Option.value_exn
                                (Option.map ~f:String.length last_lnum) in
          List.iter ~f:(fun (lnum, l) ->
            Format.fprintf pp "%*s: %s\n" padding_width lnum l)
          lines
  in
  Format.fprintf ppf
    "@{<loc>File \"%s\", line %d, characters %d-%d:@}@\n%a"
    start.pos_fname start.pos_lnum start_c stop_c
    pp_file_excerpt ()

let warn t fmt =
  Errors.kerrf ~f:print_to_console
    ("%a@{<warning>Warning@}: " ^^ fmt ^^ "@.") print t
