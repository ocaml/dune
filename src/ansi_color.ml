open Import

include struct
  [@@@warning "-37"]
  type color =
    | Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White
    | Default
    | Bright_black
    | Bright_red
    | Bright_green
    | Bright_yellow
    | Bright_blue
    | Bright_magenta
    | Bright_cyan
    | Bright_white

  type style =
    | Reset
    | Bold
    | Underlined
    | Dim
    | Blink
    | Inverse
    | Hidden
    | Bold_off
    | Underlined_off
    | Dim_off
    | Blink_off
    | Inverse_off
    | Hidden_off
    | Foreground of color
    | Background of color
end

type styles = style list

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

let stderr_supports_colors = lazy(
  not Sys.win32        &&
  Unix.(isatty stderr) &&
  match Sys.getenv "TERM" with
  | exception Not_found -> false
  | "dumb" -> false
  | _ -> true
)

let strip_colors_for_stderr s =
  if Lazy.force stderr_supports_colors then
    s
  else
    strip s

(* We redirect the output of all commands, so by default the various tools will disable
   colors. Since we support colors in the output of commands, we force it via specific
   environment variables if stderr supports colors. *)
let setup_env_for_colors = lazy(
  if !Clflags.capture_outputs && Lazy.force stderr_supports_colors then begin
    let set var value =
      match Sys.getenv var with
      | exception Not_found -> Unix.putenv var value
      | (_ : string) -> ()
    in
    set "OPAMCOLOR" "always";
    set "OCAML_COLOR" "always";
  end
)

let styles_of_tag = function
  | "loc"     -> [Bold]
  | "error"   -> [Bold; Foreground Red]
  | "warning" -> [Bold; Foreground Magenta]
  | "kwd"     -> [Bold; Foreground Blue]
  | "id"      -> [Bold; Foreground Yellow]
  | "prompt"  -> [Bold; Foreground Green]
  | "details" -> [Dim; Foreground White]
  | "ok"      -> [Dim; Foreground Green]
  | "debug"   -> [Underlined; Foreground Bright_cyan]
  | _         -> []

let setup_err_formatter_colors () =
  let open Format in
  if Lazy.force stderr_supports_colors then begin
    List.iter [err_formatter; err_ppf] ~f:(fun ppf ->
      let funcs = pp_get_formatter_tag_functions ppf () in
      pp_set_mark_tags ppf true;
      pp_set_formatter_tag_functions ppf
        { funcs with
          mark_close_tag = (fun _ -> ansi_escape_of_styles [Reset])
        ; mark_open_tag = (fun tag -> ansi_escape_of_styles (styles_of_tag tag))
        })
  end

let output_filename = [Bold; Foreground Green]
