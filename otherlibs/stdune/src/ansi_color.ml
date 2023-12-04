module RGB8 : sig
  type t

  val to_dyn : t -> Dyn.t
  val of_int : int -> t
  val to_int : t -> int
  val of_char : char -> t
  val to_char : t -> char
  val compare : t -> t -> Ordering.t

  (* This is only used internally. *)
  val write_to_buffer : Buffer.t -> t -> unit
end = struct
  type t = char

  let of_char t = t
  let to_char t = t
  let to_dyn t = Dyn.Int (int_of_char t)
  let of_int t = char_of_int (t land 0xFF)
  let to_int t = int_of_char t
  let compare t1 t2 = Char.compare t1 t2

  let write_to_buffer buf c =
    Buffer.add_string buf "38;5;";
    int_of_char c |> Int.to_string |> Buffer.add_string buf
  ;;
end

module RGB24 : sig
  type t

  val to_dyn : t -> Dyn.t
  val compare : t -> t -> Ordering.t
  val red : t -> int
  val green : t -> int
  val blue : t -> int
  val make : red:int -> green:int -> blue:int -> t
  val to_int : t -> int
  val of_int : int -> t

  (* This is only used internally. *)
  val write_to_buffer : Buffer.t -> t -> unit
end = struct
  type t = int

  let compare = Int.compare
  let red t = Int.shift_right t 16 land 0xFF
  let green t = Int.shift_right t 8 land 0xFF
  let blue t = t land 0xFF
  let to_dyn t = Dyn.list Dyn.int [ red t; green t; blue t ]
  let to_int t = t
  let of_int t = t

  let make ~red ~green ~blue =
    ((red land 0xFF) lsl 16) lor ((green land 0xFF) lsl 8) lor (blue land 0xFF)
  ;;

  let write_to_buffer buf t =
    Buffer.add_string buf "38;2;";
    red t |> Int.to_string |> Buffer.add_string buf;
    Buffer.add_char buf ';';
    green t |> Int.to_string |> Buffer.add_string buf;
    Buffer.add_char buf ';';
    blue t |> Int.to_string |> Buffer.add_string buf
  ;;
end

module Style = struct
  type t =
    [ `Fg_default
    | `Fg_black
    | `Fg_red
    | `Fg_green
    | `Fg_yellow
    | `Fg_blue
    | `Fg_magenta
    | `Fg_cyan
    | `Fg_white
    | `Fg_bright_black
    | `Fg_bright_red
    | `Fg_bright_green
    | `Fg_bright_yellow
    | `Fg_bright_blue
    | `Fg_bright_magenta
    | `Fg_bright_cyan
    | `Fg_bright_white
    | `Fg_8_bit_color of RGB8.t
    | `Fg_24_bit_color of RGB24.t
    | `Bg_default
    | `Bg_black
    | `Bg_red
    | `Bg_green
    | `Bg_yellow
    | `Bg_blue
    | `Bg_magenta
    | `Bg_cyan
    | `Bg_white
    | `Bg_bright_black
    | `Bg_bright_red
    | `Bg_bright_green
    | `Bg_bright_yellow
    | `Bg_bright_blue
    | `Bg_bright_magenta
    | `Bg_bright_cyan
    | `Bg_bright_white
    | `Bg_8_bit_color of RGB8.t
    | `Bg_24_bit_color of RGB24.t
    | `Bold
    | `Dim
    | `Italic
    | `Underline
    ]

  let write_to_buffer buf : t -> unit = function
    | `Fg_default -> Buffer.add_string buf "39"
    | `Fg_black -> Buffer.add_string buf "30"
    | `Fg_red -> Buffer.add_string buf "31"
    | `Fg_green -> Buffer.add_string buf "32"
    | `Fg_yellow -> Buffer.add_string buf "33"
    | `Fg_blue -> Buffer.add_string buf "34"
    | `Fg_magenta -> Buffer.add_string buf "35"
    | `Fg_cyan -> Buffer.add_string buf "36"
    | `Fg_white -> Buffer.add_string buf "37"
    | `Fg_bright_black -> Buffer.add_string buf "90"
    | `Fg_bright_red -> Buffer.add_string buf "91"
    | `Fg_bright_green -> Buffer.add_string buf "92"
    | `Fg_bright_yellow -> Buffer.add_string buf "93"
    | `Fg_bright_blue -> Buffer.add_string buf "94"
    | `Fg_bright_magenta -> Buffer.add_string buf "95"
    | `Fg_bright_cyan -> Buffer.add_string buf "96"
    | `Fg_bright_white -> Buffer.add_string buf "97"
    | `Fg_8_bit_color c -> RGB8.write_to_buffer buf c
    | `Fg_24_bit_color rgb -> RGB24.write_to_buffer buf rgb
    | `Bg_default -> Buffer.add_string buf "49"
    | `Bg_black -> Buffer.add_string buf "40"
    | `Bg_red -> Buffer.add_string buf "41"
    | `Bg_green -> Buffer.add_string buf "42"
    | `Bg_yellow -> Buffer.add_string buf "43"
    | `Bg_blue -> Buffer.add_string buf "44"
    | `Bg_magenta -> Buffer.add_string buf "45"
    | `Bg_cyan -> Buffer.add_string buf "46"
    | `Bg_white -> Buffer.add_string buf "47"
    | `Bg_bright_black -> Buffer.add_string buf "100"
    | `Bg_bright_red -> Buffer.add_string buf "101"
    | `Bg_bright_green -> Buffer.add_string buf "102"
    | `Bg_bright_yellow -> Buffer.add_string buf "103"
    | `Bg_bright_blue -> Buffer.add_string buf "104"
    | `Bg_bright_magenta -> Buffer.add_string buf "105"
    | `Bg_bright_cyan -> Buffer.add_string buf "106"
    | `Bg_bright_white -> Buffer.add_string buf "107"
    | `Bg_8_bit_color c -> RGB8.write_to_buffer buf c
    | `Bg_24_bit_color rgb -> RGB24.write_to_buffer buf rgb
    | `Bold -> Buffer.add_string buf "1"
    | `Dim -> Buffer.add_string buf "2"
    | `Italic -> Buffer.add_string buf "3"
    | `Underline -> Buffer.add_string buf "4"
  ;;

  let to_dyn : t -> Dyn.t = function
    | `Fg_default -> Dyn.variant "Fg_default" []
    | `Fg_black -> Dyn.variant "Fg_black" []
    | `Fg_red -> Dyn.variant "Fg_red" []
    | `Fg_green -> Dyn.variant "Fg_green" []
    | `Fg_yellow -> Dyn.variant "Fg_yellow" []
    | `Fg_blue -> Dyn.variant "Fg_blue" []
    | `Fg_magenta -> Dyn.variant "Fg_magenta" []
    | `Fg_cyan -> Dyn.variant "Fg_cyan" []
    | `Fg_white -> Dyn.variant "Fg_white" []
    | `Fg_bright_black -> Dyn.variant "Fg_bright_black" []
    | `Fg_bright_red -> Dyn.variant "Fg_bright_red" []
    | `Fg_bright_green -> Dyn.variant "Fg_bright_green" []
    | `Fg_bright_yellow -> Dyn.variant "Fg_bright_yellow" []
    | `Fg_bright_blue -> Dyn.variant "Fg_bright_blue" []
    | `Fg_bright_magenta -> Dyn.variant "Fg_bright_magenta" []
    | `Fg_bright_cyan -> Dyn.variant "Fg_bright_cyan" []
    | `Fg_bright_white -> Dyn.variant "Fg_bright_white" []
    | `Fg_8_bit_color c -> Dyn.variant "Fg_8_bit_color" [ RGB8.to_dyn c ]
    | `Fg_24_bit_color rgb -> Dyn.variant "Fg_24_bit_color" [ RGB24.to_dyn rgb ]
    | `Bg_default -> Dyn.variant "Bg_default" []
    | `Bg_black -> Dyn.variant "Bg_black" []
    | `Bg_red -> Dyn.variant "Bg_red" []
    | `Bg_green -> Dyn.variant "Bg_green" []
    | `Bg_yellow -> Dyn.variant "Bg_yellow" []
    | `Bg_blue -> Dyn.variant "Bg_blue" []
    | `Bg_magenta -> Dyn.variant "Bg_magenta" []
    | `Bg_cyan -> Dyn.variant "Bg_cyan" []
    | `Bg_white -> Dyn.variant "Bg_white" []
    | `Bg_bright_black -> Dyn.variant "Bg_bright_black" []
    | `Bg_bright_red -> Dyn.variant "Bg_bright_red" []
    | `Bg_bright_green -> Dyn.variant "Bg_bright_green" []
    | `Bg_bright_yellow -> Dyn.variant "Bg_bright_yellow" []
    | `Bg_bright_blue -> Dyn.variant "Bg_bright_blue" []
    | `Bg_bright_magenta -> Dyn.variant "Bg_bright_magenta" []
    | `Bg_bright_cyan -> Dyn.variant "Bg_bright_cyan" []
    | `Bg_bright_white -> Dyn.variant "Bg_bright_white" []
    | `Bg_8_bit_color c -> Dyn.variant "Bg_8_bit_color" [ RGB8.to_dyn c ]
    | `Bg_24_bit_color rgb -> Dyn.variant "Bg_24_bit_color" [ RGB24.to_dyn rgb ]
    | `Bold -> Dyn.variant "Bold" []
    | `Dim -> Dyn.variant "Dim" []
    | `Italic -> Dyn.variant "Italic" []
    | `Underline -> Dyn.variant "Underline" []
  ;;

  let compare (t1 : t) (t2 : t) : Ordering.t =
    match t1, t2 with
    | `Fg_default, `Fg_default -> Eq
    | `Fg_default, _ -> Lt
    | _, `Fg_default -> Gt
    | `Fg_black, `Fg_black -> Eq
    | `Fg_black, _ -> Lt
    | _, `Fg_black -> Gt
    | `Fg_red, `Fg_red -> Eq
    | `Fg_red, _ -> Lt
    | _, `Fg_red -> Gt
    | `Fg_green, `Fg_green -> Eq
    | `Fg_green, _ -> Lt
    | _, `Fg_green -> Gt
    | `Fg_yellow, `Fg_yellow -> Eq
    | `Fg_yellow, _ -> Lt
    | _, `Fg_yellow -> Gt
    | `Fg_blue, `Fg_blue -> Eq
    | `Fg_blue, _ -> Lt
    | _, `Fg_blue -> Gt
    | `Fg_magenta, `Fg_magenta -> Eq
    | `Fg_magenta, _ -> Lt
    | _, `Fg_magenta -> Gt
    | `Fg_cyan, `Fg_cyan -> Eq
    | `Fg_cyan, _ -> Lt
    | _, `Fg_cyan -> Gt
    | `Fg_white, `Fg_white -> Eq
    | `Fg_white, _ -> Lt
    | _, `Fg_white -> Gt
    | `Fg_bright_black, `Fg_bright_black -> Eq
    | `Fg_bright_black, _ -> Lt
    | _, `Fg_bright_black -> Gt
    | `Fg_bright_red, `Fg_bright_red -> Eq
    | `Fg_bright_red, _ -> Lt
    | _, `Fg_bright_red -> Gt
    | `Fg_bright_green, `Fg_bright_green -> Eq
    | `Fg_bright_green, _ -> Lt
    | _, `Fg_bright_green -> Gt
    | `Fg_bright_yellow, `Fg_bright_yellow -> Eq
    | `Fg_bright_yellow, _ -> Lt
    | _, `Fg_bright_yellow -> Gt
    | `Fg_bright_blue, `Fg_bright_blue -> Eq
    | `Fg_bright_blue, _ -> Lt
    | _, `Fg_bright_blue -> Gt
    | `Fg_bright_magenta, `Fg_bright_magenta -> Eq
    | `Fg_bright_magenta, _ -> Lt
    | _, `Fg_bright_magenta -> Gt
    | `Fg_bright_cyan, `Fg_bright_cyan -> Eq
    | `Fg_bright_cyan, _ -> Lt
    | _, `Fg_bright_cyan -> Gt
    | `Fg_bright_white, `Fg_bright_white -> Eq
    | `Fg_bright_white, _ -> Lt
    | _, `Fg_bright_white -> Gt
    | `Fg_8_bit_color c1, `Fg_8_bit_color c2 -> RGB8.compare c1 c2
    | `Fg_8_bit_color _, _ -> Lt
    | _, `Fg_8_bit_color _ -> Gt
    | `Fg_24_bit_color c1, `Fg_24_bit_color c2 -> RGB24.compare c1 c2
    | `Fg_24_bit_color _, _ -> Lt
    | _, `Fg_24_bit_color _ -> Gt
    | `Bg_default, `Bg_default -> Eq
    | `Bg_default, _ -> Lt
    | _, `Bg_default -> Gt
    | `Bg_black, `Bg_black -> Eq
    | `Bg_black, _ -> Lt
    | _, `Bg_black -> Gt
    | `Bg_red, `Bg_red -> Eq
    | `Bg_red, _ -> Lt
    | _, `Bg_red -> Gt
    | `Bg_green, `Bg_green -> Eq
    | `Bg_green, _ -> Lt
    | _, `Bg_green -> Gt
    | `Bg_yellow, `Bg_yellow -> Eq
    | `Bg_yellow, _ -> Lt
    | _, `Bg_yellow -> Gt
    | `Bg_blue, `Bg_blue -> Eq
    | `Bg_blue, _ -> Lt
    | _, `Bg_blue -> Gt
    | `Bg_magenta, `Bg_magenta -> Eq
    | `Bg_magenta, _ -> Lt
    | _, `Bg_magenta -> Gt
    | `Bg_cyan, `Bg_cyan -> Eq
    | `Bg_cyan, _ -> Lt
    | _, `Bg_cyan -> Gt
    | `Bg_white, `Bg_white -> Eq
    | `Bg_white, _ -> Lt
    | _, `Bg_white -> Gt
    | `Bg_bright_black, `Bg_bright_black -> Eq
    | `Bg_bright_black, _ -> Lt
    | _, `Bg_bright_black -> Gt
    | `Bg_bright_red, `Bg_bright_red -> Eq
    | `Bg_bright_red, _ -> Lt
    | _, `Bg_bright_red -> Gt
    | `Bg_bright_green, `Bg_bright_green -> Eq
    | `Bg_bright_green, _ -> Lt
    | _, `Bg_bright_green -> Gt
    | `Bg_bright_yellow, `Bg_bright_yellow -> Eq
    | `Bg_bright_yellow, _ -> Lt
    | _, `Bg_bright_yellow -> Gt
    | `Bg_bright_blue, `Bg_bright_blue -> Eq
    | `Bg_bright_blue, _ -> Lt
    | _, `Bg_bright_blue -> Gt
    | `Bg_bright_magenta, `Bg_bright_magenta -> Eq
    | `Bg_bright_magenta, _ -> Lt
    | _, `Bg_bright_magenta -> Gt
    | `Bg_bright_cyan, `Bg_bright_cyan -> Eq
    | `Bg_bright_cyan, _ -> Lt
    | _, `Bg_bright_cyan -> Gt
    | `Bg_bright_white, `Bg_bright_white -> Eq
    | `Bg_bright_white, _ -> Lt
    | _, `Bg_bright_white -> Gt
    | `Bg_8_bit_color c1, `Bg_8_bit_color c2 -> RGB8.compare c1 c2
    | `Bg_8_bit_color _, _ -> Lt
    | _, `Bg_8_bit_color _ -> Gt
    | `Bg_24_bit_color c1, `Bg_24_bit_color c2 -> RGB24.compare c1 c2
    | `Bg_24_bit_color _, _ -> Lt
    | _, `Bg_24_bit_color _ -> Gt
    | `Bold, `Bold -> Eq
    | `Bold, _ -> Lt
    | _, `Bold -> Gt
    | `Dim, `Dim -> Eq
    | `Dim, _ -> Lt
    | _, `Dim -> Gt
    | `Italic, `Italic -> Eq
    | `Italic, _ -> Lt
    | _, `Italic -> Gt
    | `Underline, `Underline -> Eq
  ;;

  module Of_ansi_code = struct
    type code = t

    type nonrec t =
      [ `Clear
      | `Unknown
      | code
      ]

    let write_to_buffer (buf : Buffer.t) = function
      | `Clear -> Buffer.add_char buf '0'
      | `Unknown -> Buffer.add_char buf '0'
      | #code as t -> write_to_buffer buf (t :> code)
    ;;
  end

  let of_ansi_code : int -> Of_ansi_code.t = function
    | 39 -> `Fg_default
    | 30 -> `Fg_black
    | 31 -> `Fg_red
    | 32 -> `Fg_green
    | 33 -> `Fg_yellow
    | 34 -> `Fg_blue
    | 35 -> `Fg_magenta
    | 36 -> `Fg_cyan
    | 37 -> `Fg_white
    | 90 -> `Fg_bright_black
    | 91 -> `Fg_bright_red
    | 92 -> `Fg_bright_green
    | 93 -> `Fg_bright_yellow
    | 94 -> `Fg_bright_blue
    | 95 -> `Fg_bright_magenta
    | 96 -> `Fg_bright_cyan
    | 97 -> `Fg_bright_white
    | 49 -> `Bg_default
    | 40 -> `Bg_black
    | 41 -> `Bg_red
    | 42 -> `Bg_green
    | 43 -> `Bg_yellow
    | 44 -> `Bg_blue
    | 45 -> `Bg_magenta
    | 46 -> `Bg_cyan
    | 47 -> `Bg_white
    | 100 -> `Bg_bright_black
    | 101 -> `Bg_bright_red
    | 102 -> `Bg_bright_green
    | 103 -> `Bg_bright_yellow
    | 104 -> `Bg_bright_blue
    | 105 -> `Bg_bright_magenta
    | 106 -> `Bg_bright_cyan
    | 107 -> `Bg_bright_white
    | 1 -> `Bold
    | 2 -> `Dim
    | 3 -> `Italic
    | 4 -> `Underline
    | 0 -> `Clear
    | _ -> `Unknown
  ;;

  let is_not_fg = function
    | `Fg_default
    | `Fg_black
    | `Fg_red
    | `Fg_green
    | `Fg_yellow
    | `Fg_blue
    | `Fg_magenta
    | `Fg_cyan
    | `Fg_white
    | `Fg_bright_black
    | `Fg_bright_red
    | `Fg_bright_green
    | `Fg_bright_yellow
    | `Fg_bright_blue
    | `Fg_bright_magenta
    | `Fg_bright_cyan
    | `Fg_bright_white
    | `Fg_8_bit_color _
    | `Fg_24_bit_color _ -> false
    | _ -> true
  ;;

  let is_not_bg = function
    | `Bg_default
    | `Bg_black
    | `Bg_red
    | `Bg_green
    | `Bg_yellow
    | `Bg_blue
    | `Bg_magenta
    | `Bg_cyan
    | `Bg_white
    | `Bg_bright_black
    | `Bg_bright_red
    | `Bg_bright_green
    | `Bg_bright_yellow
    | `Bg_bright_blue
    | `Bg_bright_magenta
    | `Bg_bright_cyan
    | `Bg_bright_white
    | `Bg_8_bit_color _
    | `Bg_24_bit_color _ -> false
    | _ -> true
  ;;

  let rec write_codes buf = function
    | [] -> ()
    | [ t ] -> Of_ansi_code.write_to_buffer buf t
    | t :: ts ->
      Of_ansi_code.write_to_buffer buf t;
      Buffer.add_char buf ';';
      write_codes buf ts
  ;;

  let escape_sequence_no_reset buf l =
    Buffer.add_string buf "\027[";
    write_codes buf l;
    Buffer.add_char buf 'm';
    let res = Buffer.contents buf in
    Buffer.clear buf;
    res
  ;;

  let escape_sequence_buf buf l =
    escape_sequence_no_reset buf (`Clear :: (l :> Of_ansi_code.t list))
  ;;

  let escape_sequence (l : t list) =
    escape_sequence_buf (Buffer.create 16) (l :> Of_ansi_code.t list)
  ;;
end

let supports_color isatty =
  let is_smart =
    match Env.(get initial) "TERM" with
    | Some "dumb" -> false
    | _ -> true
  and clicolor =
    match Env.(get initial) "CLICOLOR" with
    | Some "0" -> false
    | _ -> true
  and clicolor_force =
    match Env.(get initial) "CLICOLOR_FORCE" with
    | None | Some "0" -> false
    | _ -> true
  in
  clicolor_force || (is_smart && clicolor && Lazy.force isatty)
;;

let stdout_supports_color = lazy (supports_color (lazy (Unix.isatty Unix.stdout)))
let output_is_a_tty = lazy (Unix.isatty Unix.stderr)
let stderr_supports_color = lazy (supports_color output_is_a_tty)

let rec tag_handler buf current_styles ppf (styles : Style.t list) pp =
  Format.pp_print_as
    ppf
    0
    (Style.escape_sequence_no_reset buf (styles :> Style.Of_ansi_code.t list));
  Pp.to_fmt_with_tags ppf pp ~tag_handler:(tag_handler buf (current_styles @ styles));
  Format.pp_print_as
    ppf
    0
    (Style.escape_sequence_buf buf (current_styles :> Style.t list))
;;

let make_printer supports_color ppf =
  let f =
    lazy
      (if Lazy.force supports_color
       then (
         let buf = Buffer.create 16 in
         Pp.to_fmt_with_tags ppf ~tag_handler:(tag_handler buf []))
       else Pp.to_fmt ppf)
  in
  Staged.stage (fun pp ->
    Lazy.force f pp;
    Format.pp_print_flush ppf ())
;;

let print = Staged.unstage (make_printer stdout_supports_color Format.std_formatter)
let prerr = Staged.unstage (make_printer stderr_supports_color Format.err_formatter)

let strip str =
  let len = String.length str in
  let buf = Buffer.create len in
  let rec loop start i =
    if i = len
    then (
      if i - start > 0 then Buffer.add_substring buf str start (i - start);
      Buffer.contents buf)
    else (
      match String.unsafe_get str i with
      | '\027' ->
        if i - start > 0 then Buffer.add_substring buf str start (i - start);
        skip (i + 1)
      | _ -> loop start (i + 1))
  and skip i =
    if i = len
    then Buffer.contents buf
    else (
      match String.unsafe_get str i with
      | 'm' -> loop (i + 1) (i + 1)
      | _ -> skip (i + 1))
  in
  loop 0 0
;;

let index_from_any str start chars =
  let n = String.length str in
  let rec go i =
    if i >= n
    then None
    else (
      match List.find chars ~f:(fun c -> Char.equal str.[i] c) with
      | None -> go (i + 1)
      | Some c -> Some (i, c))
  in
  go start
;;

let rec parse_styles l (accu : Style.t list) =
  (* This function takes a list of strings, taken from splitting an Ansi code on
     ';', and adds the parsed styles to the already accumulated styles. There is
     some non-trivial interaction with parsing here. For example, 8-bit and
     24-bit color codes need some lookahead and default colours need to be able
     to override other styles. *)
  match l with
  | [] -> accu (* Parsing 8-bit foreground colors *)
  | "38" :: "5" :: s :: l ->
    parse_styles
      l
      (match Int.of_string s with
       | None -> accu
       | Some code -> `Fg_8_bit_color (RGB8.of_int code) :: accu)
  (* Parsing 8-bit background colors *)
  | "48" :: "5" :: s :: l ->
    parse_styles
      l
      (match Int.of_string s with
       | None -> accu
       | Some code -> `Bg_8_bit_color (RGB8.of_int code) :: accu)
  (* Parsing 24-bit foreground colors *)
  | "38" :: "2" :: r :: g :: b :: l ->
    parse_styles
      l
      (match Int.of_string r, Int.of_string g, Int.of_string b with
       | Some red, Some green, Some blue ->
         `Fg_24_bit_color (RGB24.make ~red ~green ~blue) :: accu
       | _ -> accu)
  (* Parsing 24-bit background colors *)
  | "48" :: "2" :: r :: g :: b :: l ->
    parse_styles
      l
      (match Int.of_string r, Int.of_string g, Int.of_string b with
       | Some red, Some green, Some blue ->
         `Bg_24_bit_color (RGB24.make ~red ~green ~blue) :: accu
       | _ -> accu)
  | s :: l ->
    parse_styles
      l
      (match Int.of_string s with
       | None -> accu
       | Some code ->
         (match Style.of_ansi_code code with
          | `Clear -> []
          | `Unknown -> accu
          (* If the foreground is set to default, we filter out any
             other foreground modifiers. Same for background. *)
          | `Fg_default -> List.filter accu ~f:Style.is_not_fg
          | `Bg_default -> List.filter accu ~f:Style.is_not_bg
          | #Style.t as s -> s :: accu))
;;

let parse_styles styles l = parse_styles l (List.rev styles) |> List.rev

let parse_line str styles =
  let len = String.length str in
  let add_chunk acc ~styles ~pos ~len =
    if len = 0
    then acc
    else (
      let s = Pp.verbatim (String.sub str ~pos ~len) in
      let s =
        match styles with
        | [] -> s
        | _ -> Pp.tag styles s
      in
      Pp.seq acc s)
  in
  let rec loop (styles : Style.t list) i acc =
    match String.index_from str i '\027' with
    | None -> styles, add_chunk acc ~styles ~pos:i ~len:(len - i)
    | Some seq_start ->
      let acc = add_chunk acc ~styles ~pos:i ~len:(seq_start - i) in
      (* Skip the "\027[" *)
      let seq_start = seq_start + 2 in
      if seq_start >= len || str.[seq_start - 1] <> '['
      then styles, acc
      else (
        match index_from_any str seq_start [ 'm'; 'K' ] with
        | None -> styles, acc
        | Some (seq_end, 'm') ->
          let styles =
            if seq_start = seq_end
            then
              (* Some commands output "\027[m", which seems to be interpreted
                 the same as "\027[0m" by terminals *)
              []
            else
              String.sub str ~pos:seq_start ~len:(seq_end - seq_start)
              |> String.split ~on:';'
              |> parse_styles styles
          in
          loop styles (seq_end + 1) acc
        | Some (seq_end, _) -> loop styles (seq_end + 1) acc)
  in
  loop styles 0 Pp.nop
;;

let parse =
  let rec loop styles lines acc =
    match lines with
    | [] -> Pp.vbox (Pp.concat ~sep:Pp.cut (List.rev acc))
    | line :: lines ->
      let styles, pp = parse_line line styles in
      loop styles lines (pp :: acc)
  in
  fun str -> loop [] (String.split_lines str) []
;;
