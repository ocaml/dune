module Style = struct
  type t = string

  let to_dyn s = Dyn.string s

  let fg_black = "30"

  let fg_red = "31"

  let fg_green = "32"

  let fg_yellow = "33"

  let fg_blue = "34"

  let fg_magenta = "35"

  let fg_cyan = "36"

  let fg_white = "37"

  let fg_default = "39"

  let fg_bright_black = "90"

  let fg_bright_red = "91"

  let fg_bright_green = "92"

  let fg_bright_yellow = "93"

  let fg_bright_blue = "94"

  let fg_bright_magenta = "95"

  let fg_bright_cyan = "96"

  let fg_bright_white = "97"

  let fg_all =
    [ fg_black
    ; fg_green
    ; fg_yellow
    ; fg_blue
    ; fg_magenta
    ; fg_cyan
    ; fg_white
    ; fg_bright_black
    ; fg_bright_red
    ; fg_bright_green
    ; fg_bright_yellow
    ; fg_bright_blue
    ; fg_bright_magenta
    ; fg_bright_cyan
    ; fg_bright_white
    ]

  let bg_black = "40"

  let bg_red = "41"

  let bg_green = "42"

  let bg_yellow = "43"

  let bg_blue = "44"

  let bg_magenta = "45"

  let bg_cyan = "46"

  let bg_white = "47"

  let bg_default = "49"

  let bg_bright_black = "100"

  let bg_bright_red = "101"

  let bg_bright_green = "102"

  let bg_bright_yellow = "103"

  let bg_bright_blue = "104"

  let bg_bright_magenta = "105"

  let bg_bright_cyan = "106"

  let bg_bright_white = "107"

  let bg_all =
    [ bg_black
    ; bg_red
    ; bg_green
    ; bg_yellow
    ; bg_blue
    ; bg_magenta
    ; bg_cyan
    ; bg_white
    ; bg_default
    ; bg_bright_black
    ; bg_bright_red
    ; bg_bright_green
    ; bg_bright_yellow
    ; bg_bright_blue
    ; bg_bright_magenta
    ; bg_bright_cyan
    ; bg_bright_white
    ]

  let bold = "1"

  let dim = "2"

  let underlined = "4"

  let escape_sequence l =
    let l = "0" :: l in
    Printf.sprintf "\027[%sm" (String.concat l ~sep:";")

  let escape_sequence_no_reset l =
    Printf.sprintf "\027[%sm" (String.concat l ~sep:";")
end

let supports_color fd =
  let is_smart =
    match Stdlib.Sys.getenv "TERM" with
    | exception Not_found -> false
    | "dumb" -> false
    | _ -> true
  and clicolor =
    match Stdlib.Sys.getenv "CLICOLOR" with
    | exception Not_found -> true
    | "0" -> false
    | _ -> true
  and clicolor_force =
    match Stdlib.Sys.getenv "CLICOLOR_FORCE" with
    | exception Not_found -> false
    | "0" -> false
    | _ -> true
  in
  (is_smart && Unix.isatty fd && clicolor) || clicolor_force

let stdout_supports_color = lazy (supports_color Unix.stdout)

let stderr_supports_color = lazy (supports_color Unix.stderr)

let rec tag_handler current_styles ppf styles pp =
  Format.pp_print_as ppf 0 (Style.escape_sequence_no_reset styles);
  Pp.to_fmt_with_tags ppf pp
    ~tag_handler:(tag_handler (current_styles @ styles));
  Format.pp_print_as ppf 0 (Style.escape_sequence current_styles)

let make_printer supports_color ppf =
  let f =
    lazy
      (if Lazy.force supports_color then
       Pp.to_fmt_with_tags ppf ~tag_handler:(tag_handler [])
      else Pp.to_fmt ppf)
  in
  Staged.stage (fun pp ->
      Lazy.force f pp;
      Format.pp_print_flush ppf ())

let print =
  Staged.unstage (make_printer stdout_supports_color Format.std_formatter)

let prerr =
  Staged.unstage (make_printer stderr_supports_color Format.err_formatter)

let strip str =
  let len = String.length str in
  let buf = Buffer.create len in
  let rec loop start i =
    if i = len then (
      if i - start > 0 then Buffer.add_substring buf str start (i - start);
      Buffer.contents buf)
    else
      match String.unsafe_get str i with
      | '\027' ->
        if i - start > 0 then Buffer.add_substring buf str start (i - start);
        skip (i + 1)
      | _ -> loop start (i + 1)
  and skip i =
    if i = len then Buffer.contents buf
    else
      match String.unsafe_get str i with
      | 'm' -> loop (i + 1) (i + 1)
      | _ -> skip (i + 1)
  in
  loop 0 0

let index_from_any str start chars =
  let n = String.length str in
  let rec go i =
    if i >= n then None
    else
      match List.find chars ~f:(fun c -> Char.equal str.[i] c) with
      | None -> go (i + 1)
      | Some c -> Some (i, c)
  in
  go start

let parse_line str styles =
  let len = String.length str in
  let add_chunk acc ~styles ~pos ~len =
    if len = 0 then acc
    else
      let s = Pp.verbatim (String.sub str ~pos ~len) in
      let s =
        match styles with
        | [] -> s
        | _ -> Pp.tag styles s
      in
      Pp.seq acc s
  in
  let rec loop styles i acc =
    match String.index_from str i '\027' with
    | None -> (styles, add_chunk acc ~styles ~pos:i ~len:(len - i))
    | Some seq_start -> (
      let acc = add_chunk acc ~styles ~pos:i ~len:(seq_start - i) in
      (* Skip the "\027[" *)
      let seq_start = seq_start + 2 in
      if seq_start >= len || str.[seq_start - 1] <> '[' then (styles, acc)
      else
        match index_from_any str seq_start [ 'm'; 'K' ] with
        | None -> (styles, acc)
        | Some (seq_end, 'm') ->
          let styles =
            if seq_start = seq_end then
              (* Some commands output "\027[m", which seems to be interpreted
                 the same as "\027[0m" by terminals *)
              []
            else
              String.sub str ~pos:seq_start ~len:(seq_end - seq_start)
              |> String.split ~on:';'
              |> List.fold_left ~init:(List.rev styles) ~f:(fun styles s ->
                     if s = Style.fg_default then
                       List.filter styles ~f:(fun s ->
                           not (List.mem Style.fg_all s ~equal:String.equal))
                     else if s = Style.bg_default then
                       List.filter styles ~f:(fun s ->
                           not (List.mem Style.bg_all s ~equal:String.equal))
                     else if s = "0" then []
                     else s :: styles)
              |> List.rev
          in
          loop styles (seq_end + 1) acc
        | Some (seq_end, _) -> loop styles (seq_end + 1) acc)
  in
  loop styles 0 Pp.nop

let parse =
  let rec loop styles lines acc =
    match lines with
    | [] -> Pp.vbox (Pp.concat ~sep:Pp.cut (List.rev acc))
    | line :: lines ->
      let styles, pp = parse_line line styles in
      loop styles lines (pp :: acc)
  in
  fun str -> loop [] (String.split_lines str) []
