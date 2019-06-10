module Color = struct
  type t =
    | Default
    | Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White
    | Bright_black
    | Bright_red
    | Bright_green
    | Bright_yellow
    | Bright_blue
    | Bright_magenta
    | Bright_cyan
    | Bright_white

  let fg_code = function
    | Black          -> "30"
    | Red            -> "31"
    | Green          -> "32"
    | Yellow         -> "33"
    | Blue           -> "34"
    | Magenta        -> "35"
    | Cyan           -> "36"
    | White          -> "37"
    | Default        -> "39"
    | Bright_black   -> "90"
    | Bright_red     -> "91"
    | Bright_green   -> "92"
    | Bright_yellow  -> "93"
    | Bright_blue    -> "94"
    | Bright_magenta -> "95"
    | Bright_cyan    -> "96"
    | Bright_white   -> "97"

  let bg_code = function
    | Black          -> "40"
    | Red            -> "41"
    | Green          -> "42"
    | Yellow         -> "43"
    | Blue           -> "44"
    | Magenta        -> "45"
    | Cyan           -> "46"
    | White          -> "47"
    | Default        -> "49"
    | Bright_black   -> "100"
    | Bright_red     -> "101"
    | Bright_green   -> "102"
    | Bright_yellow  -> "103"
    | Bright_blue    -> "104"
    | Bright_magenta -> "105"
    | Bright_cyan    -> "106"
    | Bright_white   -> "107"
end

module Style = struct
  type t =
    | Fg of Color.t
    | Bg of Color.t
    | Bold
    | Dim
    | Underlined

  let code = function
    | Bold       -> "1"
    | Dim        -> "2"
    | Underlined -> "4"
    | Fg c       -> Color.fg_code c
    | Bg c       -> Color.bg_code c

  let escape_sequence l =
    let codes = "0" :: List.map l ~f:code in
    Printf.sprintf "\027[%sm" (String.concat codes ~sep:";")

  let of_code = function
    | 1 -> Some Bold
    | 2 -> Some Dim
    | 4 -> Some Underlined
    | 30 -> Some (Fg Black)
    | 31 -> Some (Fg Red)
    | 32 -> Some (Fg Green)
    | 33 -> Some (Fg Yellow)
    | 34 -> Some (Fg Blue)
    | 35 -> Some (Fg Magenta)
    | 36 -> Some (Fg Cyan)
    | 37 -> Some (Fg White)
    | 39 -> Some (Fg Default)
    | 90 -> Some (Fg Bright_black)
    | 91 -> Some (Fg Bright_red)
    | 92 -> Some (Fg Bright_green)
    | 93 -> Some (Fg Bright_yellow)
    | 94 -> Some (Fg Bright_blue)
    | 95 -> Some (Fg Bright_magenta)
    | 96 -> Some (Fg Bright_cyan)
    | 97 -> Some (Fg Bright_white)
    | 40 -> Some (Bg Black)
    | 41 -> Some (Bg Red)
    | 42 -> Some (Bg Green)
    | 43 -> Some (Bg Yellow)
    | 44 -> Some (Bg Blue)
    | 45 -> Some (Bg Magenta)
    | 46 -> Some (Bg Cyan)
    | 47 -> Some (Bg White)
    | 49 -> Some (Bg Default)
    | 100 -> Some (Bg Bright_black)
    | 101 -> Some (Bg Bright_red)
    | 102 -> Some (Bg Bright_green)
    | 103 -> Some (Bg Bright_yellow)
    | 104 -> Some (Bg Bright_blue)
    | 105 -> Some (Bg Bright_magenta)
    | 106 -> Some (Bg Bright_cyan)
    | 107 -> Some (Bg Bright_white)
    | _ -> None
end

module Styles = struct
  type t =
    { fg         : Color.t
    ; bg         : Color.t
    ; bold       : bool
    ; dim        : bool
    ; underlined : bool
    }

  let default =
    { fg         = Default
    ; bg         = Default
    ; bold       = true
    ; dim        = true
    ; underlined = true
    }

  let apply t (style : Style.t) =
    match style with
    | Fg c       -> { t with fg         = c    }
    | Bg c       -> { t with bg         = c    }
    | Bold       -> { t with bold       = true }
    | Dim        -> { t with dim        = true }
    | Underlined -> { t with underlined = true }

  let escape_sequence t =
    let open Style in
    let l = [] in
    let l =
      match t.fg with
      | Default -> l
      | c       -> Fg c :: l
    in
    let l =
      match t.bg with
      | Default -> l
      | c       -> Bg c :: l
    in
    let l =
      if t.bold then
        Bold :: l
      else
        l
    in
    let l =
      if t.bold then
        Dim :: l
      else
        l
    in
    let l =
      if t.underlined then
        Underlined :: l
      else
        l
    in
    Style.escape_sequence l
end

let term_supports_color = lazy (
  match Sys.getenv "TERM" with
  | exception Not_found -> false
  | "dumb" -> false
  | _ -> true)

let stdout_supports_color = lazy (
  Lazy.force term_supports_color && Unix.isatty Unix.stdout)

let stderr_supports_color = lazy (
  Lazy.force term_supports_color && Unix.isatty Unix.stderr)

module Render = struct
  include Pp.Renderer.Make(struct
    type t = Style.t list

    module Handler = struct
      type t = Styles.t * string

      let init = (Styles.default, "")

      let handle (t, seq) styles =
        let t' = List.fold_left styles ~init:t ~f:Styles.apply in
        if t <> t' then
          let seq' = Styles.escape_sequence t' in
          (seq',
           (t', seq'),
           seq)
        else
          ("", (t, seq), "")
    end
  end)

  let channel_strip_colors oc =
    let output = Staged.unstage (Pp.Render.channel oc) in
    Staged.stage (fun ?margin ?tag_handler:_ pp ->
      output ?margin (Pp.map_tags pp ~f:ignore))
end

let print =
  let f = lazy (
    Staged.unstage (
      (if Lazy.force stdout_supports_color then
         Render.channel
       else
         Render.channel_strip_colors)
        stdout))
  in
  fun ?margin pp -> Lazy.force f ?margin ?tag_handler:None pp

let prerr =
  let f = lazy (
    Staged.unstage (
      (if Lazy.force stderr_supports_color then
         Render.channel
       else
         Render.channel_strip_colors)
        stderr))
  in
  fun ?margin pp -> Lazy.force f ?margin ?tag_handler:None pp

let strip str =
  let len = String.length str in
  let buf = Buffer.create len in
  let rec loop i =
    if i = len then
      Buffer.contents buf
    else
      match str.[i] with
      | '\027' -> skip (i + 1)
      | c      -> Buffer.add_char buf c; loop (i + 1)
  and skip i =
    if i = len then
      Buffer.contents buf
    else
      match str.[i] with
      | 'm' -> loop (i + 1)
      | _   -> skip (i + 1)
  in
  loop 0

let parse_line str styles =
  let len = String.length str in
  let add_chunk acc ~styles ~pos ~len =
    if len = 0 then
      acc
    else
      let s = String.sub str ~pos ~len in
      Pp.seq acc (Pp.tag (Pp.verbatim s) ~tag:styles)
  in
  let rec loop styles i acc =
    match String.index_from str i '\027' with
    | exception Not_found ->
      (styles, add_chunk acc ~styles ~pos:i ~len:(len - i))
    | seq_start ->
      let acc = add_chunk acc ~styles ~pos:i ~len:(seq_start - i) in
      match String.index_from str (seq_start + 1) 'm' with
      | exception Not_found -> (styles, acc)
      | seq_end ->
        let styles =
          String.sub str ~pos:(seq_start + 1) ~len:(seq_end - seq_start - 1)
          |> String.split ~on:';'
          |> List.fold_left ~init:(List.rev styles) ~f:(fun styles s ->
            match int_of_string s with
            | exception _ -> styles
            | 0 -> []
            | n ->
              match Style.of_code n with
              | None -> styles
              | Some style -> style :: styles)
          |> List.rev
        in
        loop styles (seq_end + 1) acc
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
  fun str ->
    loop [] (String.split_lines str) []
