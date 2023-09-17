open Import

let attr_of_ansi_color_rgb8 (c : Ansi_color.RGB8.t) =
  match Ansi_color.RGB8.to_int c with
  | 0 -> A.black
  | 1 -> A.red
  | 2 -> A.green
  | 3 -> A.yellow
  | 4 -> A.blue
  | 5 -> A.magenta
  | 6 -> A.cyan
  | 7 -> A.white
  | 8 -> A.lightblack
  | 9 -> A.lightred
  | 10 -> A.lightgreen
  | 11 -> A.lightyellow
  | 12 -> A.lightblue
  | 13 -> A.lightmagenta
  | 14 -> A.lightcyan
  | 15 -> A.lightwhite
  | i when i <= 231 ->
    let i = i - 16 in
    let r = i / 36 in
    let g = i / 6 mod 6 in
    let b = i mod 6 in
    A.rgb ~r ~g ~b
  | i when i <= 255 -> A.gray (i - 232)
  | i -> Code_error.raise "invalid 8-bit color" [ "value", Dyn.int i ]
;;

let attr_of_ansi_color_rgb24 (c : Ansi_color.RGB24.t) =
  A.rgb_888
    ~r:(Ansi_color.RGB24.red c)
    ~g:(Ansi_color.RGB24.green c)
    ~b:(Ansi_color.RGB24.blue c)
;;

let attr_of_ansi_color_style (s : Ansi_color.Style.t) =
  match s with
  | `Fg_black -> A.(fg black)
  | `Fg_red -> A.(fg red)
  | `Fg_green -> A.(fg green)
  | `Fg_yellow -> A.(fg yellow)
  | `Fg_blue -> A.(fg blue)
  | `Fg_magenta -> A.(fg magenta)
  | `Fg_cyan -> A.(fg cyan)
  | `Fg_white -> A.(fg white)
  | `Fg_default -> A.empty
  | `Fg_bright_black -> A.(fg lightblack)
  | `Fg_bright_red -> A.(fg lightred)
  | `Fg_bright_green -> A.(fg lightgreen)
  | `Fg_bright_yellow -> A.(fg lightyellow)
  | `Fg_bright_blue -> A.(fg lightblue)
  | `Fg_bright_magenta -> A.(fg lightmagenta)
  | `Fg_bright_cyan -> A.(fg lightcyan)
  | `Fg_bright_white -> A.(fg lightwhite)
  | `Fg_8_bit_color c -> A.fg (attr_of_ansi_color_rgb8 c)
  | `Fg_24_bit_color c -> A.fg (attr_of_ansi_color_rgb24 c)
  | `Bg_black -> A.(bg black)
  | `Bg_red -> A.(bg red)
  | `Bg_green -> A.(bg green)
  | `Bg_yellow -> A.(bg yellow)
  | `Bg_blue -> A.(bg blue)
  | `Bg_magenta -> A.(bg magenta)
  | `Bg_cyan -> A.(bg cyan)
  | `Bg_white -> A.(bg white)
  | `Bg_default -> A.empty
  | `Bg_bright_black -> A.(bg lightblack)
  | `Bg_bright_red -> A.(bg lightred)
  | `Bg_bright_green -> A.(bg lightgreen)
  | `Bg_bright_yellow -> A.(bg lightyellow)
  | `Bg_bright_blue -> A.(bg lightblue)
  | `Bg_bright_magenta -> A.(bg lightmagenta)
  | `Bg_bright_cyan -> A.(bg lightcyan)
  | `Bg_bright_white -> A.(bg lightwhite)
  | `Bg_8_bit_color c -> A.bg (attr_of_ansi_color_rgb8 c)
  | `Bg_24_bit_color c -> A.bg (attr_of_ansi_color_rgb24 c)
  | `Bold -> A.(st bold)
  | `Italic -> A.(st italic)
  | `Dim -> A.(st dim)
  | `Underline -> A.(st underline)
;;

let attr_of_user_message_style fmt t (pp : User_message.Style.t Pp.t) : unit =
  let attr =
    match (t : User_message.Style.t) with
    | Loc -> A.(st bold)
    | Error -> A.(st bold ++ fg red)
    | Warning -> A.(st bold ++ fg magenta)
    | Kwd -> A.(st bold ++ fg blue)
    | Id -> A.(st bold ++ fg yellow)
    | Prompt -> A.(st bold ++ fg green)
    | Hint -> A.(st italic ++ fg white)
    | Details -> A.(st dim ++ fg white)
    | Ok -> A.(st italic ++ fg green)
    | Debug -> A.(st underline ++ fg lightcyan)
    | Success -> A.(st bold ++ fg green)
    | Ansi_styles l ->
      List.fold_left ~init:A.empty l ~f:(fun attr s ->
        A.(attr ++ attr_of_ansi_color_style s))
  in
  Notty.I.pp_attr attr Pp.to_fmt fmt pp
;;

let pp_to_image =
  Notty.I.strf "%a" (Pp.to_fmt_with_tags ~tag_handler:attr_of_user_message_style)
;;

module Unicode = struct
  let ogham_feather_mark = Uchar.of_int 0x169B
  let ogham_reversed_feather_mark = Uchar.of_int 0x169C
  let horizontal_bar = Uchar.of_int 0x2015
  let box_drawings_double_horizontal = Uchar.of_int 0x2550
  let box_drawings_double_vertical = Uchar.of_int 0x2551
  let box_drawings_double_down_and_right = Uchar.of_int 0x2554
  let box_drawings_double_down_and_left = Uchar.of_int 0x2557
  let box_drawings_double_up_and_right = Uchar.of_int 0x255A
  let box_drawings_double_up_and_left = Uchar.of_int 0x255D
  let box_drawings_vertical_single_and_right_double = Uchar.of_int 0x255E
  let box_drawings_vertical_single_and_left_double = Uchar.of_int 0x2561
end

let horizontal_rule ~attr ~w = I.uchar attr Unicode.horizontal_bar w 1

let border_box ~attr image =
  let w, h = I.(width image, height image) in
  let border_element ?(width = 1) ?(height = 1) uchar valign halign =
    I.uchar attr uchar width height
    |> I.vsnap ~align:valign (h + 2)
    |> I.hsnap ~align:halign (w + 2)
  in
  I.zcat
    [ border_element Unicode.box_drawings_double_down_and_right `Top `Left
    ; border_element Unicode.box_drawings_double_down_and_left `Top `Right
    ; border_element Unicode.box_drawings_double_up_and_right `Bottom `Left
    ; border_element Unicode.box_drawings_double_up_and_left `Bottom `Right
    ; border_element Unicode.box_drawings_double_horizontal ~width:w `Top `Middle
    ; border_element Unicode.box_drawings_double_horizontal ~width:w `Bottom `Middle
    ; border_element Unicode.box_drawings_double_vertical ~height:h `Middle `Left
    ; border_element Unicode.box_drawings_double_vertical ~height:h `Middle `Right
    ; I.pad ~l:1 ~t:1 ~r:1 ~b:1 image
    ; I.char A.empty ' ' (w + 2) (h + 2)
    ]
;;

let box_with_title ~attr ~title ~title_attr image =
  let title =
    [ I.uchar attr Unicode.box_drawings_vertical_single_and_left_double 1 1
    ; I.string title_attr (" " ^ title ^ " ")
    ; I.uchar attr Unicode.box_drawings_vertical_single_and_right_double 1 1
    ]
    |> I.hcat
    |> I.hsnap ~align:`Middle (I.width image + 2)
    |> I.vsnap ~align:`Top (I.height image + 2)
  in
  I.(title </> border_box ~attr image)
;;
