open Import

include struct
  [@@@warning "-37"]
  type color =
    | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White | Default
    | Bright_black | Bright_red | Bright_green | Bright_yellow | Bright_blue
    | Bright_magenta | Bright_cyan | Bright_white

  type style =
    | Reset | Bold | Underlined | Dim | Blink | Inverse | Hidden
    | Bold_off | Underlined_off | Dim_off | Blink_off | Inverse_off | Hidden_off
    | Foreground of color
    | Background of color
end

let ansi_code_of_style = function
  | Reset                     -> "0"
  | Bold                      -> "1"
  | Bold_off                  -> "22"
  | Dim                       -> "2"
  | Dim_off                   -> "22"
  | Underlined                -> "4"
  | Underlined_off            -> "24"
  | Blink                     -> "5"
  | Blink_off                 -> "25"
  | Inverse                   -> "7"
  | Inverse_off               -> "27"
  | Hidden                    -> "8"
  | Hidden_off                -> "28"
  | Foreground Black          -> "30"
  | Foreground Red            -> "31"
  | Foreground Green          -> "32"
  | Foreground Yellow         -> "33"
  | Foreground Blue           -> "34"
  | Foreground Magenta        -> "35"
  | Foreground Cyan           -> "36"
  | Foreground White          -> "37"
  | Foreground Default        -> "39"
  | Foreground Bright_black   -> "90"
  | Foreground Bright_red     -> "91"
  | Foreground Bright_green   -> "92"
  | Foreground Bright_yellow  -> "93"
  | Foreground Bright_blue    -> "94"
  | Foreground Bright_magenta -> "95"
  | Foreground Bright_cyan    -> "96"
  | Foreground Bright_white   -> "97"
  | Background Black          -> "40"
  | Background Red            -> "41"
  | Background Green          -> "42"
  | Background Yellow         -> "43"
  | Background Blue           -> "44"
  | Background Magenta        -> "45"
  | Background Cyan           -> "46"
  | Background White          -> "47"
  | Background Default        -> "49"
  | Background Bright_black   -> "100"
  | Background Bright_red     -> "101"
  | Background Bright_green   -> "102"
  | Background Bright_yellow  -> "103"
  | Background Bright_blue    -> "104"
  | Background Bright_magenta -> "105"
  | Background Bright_cyan    -> "106"
  | Background Bright_white   -> "107"

let ansi_escape_of_styles styles =
  sprintf "\027[%sm"
    (List.map styles ~f:ansi_code_of_style
     |> String.concat ~sep:";")

let apply_string styles str =
  sprintf "%s%s%s" (ansi_escape_of_styles styles) str (ansi_escape_of_styles [Reset])

let colorize =
  let color_combos =
    [| Blue,          Bright_green
     ; Red,           Bright_yellow
     ; Yellow,        Blue
     ; Magenta,       Bright_cyan
     ; Bright_green,  Blue
     ; Bright_yellow, Red
     ; Blue,          Yellow
     ; Bright_cyan,   Magenta
    |]
  in
  fun ~key str ->
    let hash = Hashtbl.hash key in
    let fore, back = color_combos.(hash mod (Array.length color_combos)) in
    apply_string [Foreground fore; Background back] str

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
