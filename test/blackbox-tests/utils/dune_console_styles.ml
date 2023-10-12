open Stdune
module Console = Dune_console

let () =
  Dune_config.Config.init String.Map.empty;
  Path.set_root (Path.External.cwd ());
  Path.Build.set_build_dir (Path.Outside_build_dir.of_string "_build");
  Dune_util.Log.init ()
;;

let most_user_message_styles =
  User_message.Style.
    [ Loc; Error; Warning; Kwd; Id; Prompt; Hint; Details; Ok; Debug; Success ]
;;

let user_message_styles_message =
  let pp_self_styled style =
    Pp.tag style @@ Pp.text (User_message.Style.to_dyn style |> Dyn.to_string)
  in
  Pp.concat
    ~sep:Pp.newline
    [ Pp.tag (User_message.Style.Ansi_styles [ `Bold ])
      @@ Pp.hbox
      @@ Pp.text "Dune Console User_message Styles:"
    ; Pp.enumerate ~f:pp_self_styled most_user_message_styles
    ]
;;

let most_ansi_colors =
  [ `Fg_black
  ; `Fg_red
  ; `Fg_green
  ; `Fg_yellow
  ; `Fg_blue
  ; `Fg_magenta
  ; `Fg_cyan
  ; `Fg_white
  ; `Fg_bright_black
  ; `Fg_bright_red
  ; `Fg_bright_green
  ; `Fg_bright_yellow
  ; `Fg_bright_blue
  ; `Fg_bright_magenta
  ; `Fg_bright_cyan
  ; `Fg_bright_white
  ; `Bg_default
  ; `Bg_black
  ; `Bg_red
  ; `Bg_green
  ; `Bg_yellow
  ; `Bg_blue
  ; `Bg_magenta
  ; `Bg_cyan
  ; `Bg_white
  ; `Bg_bright_black
  ; `Bg_bright_red
  ; `Bg_bright_green
  ; `Bg_bright_yellow
  ; `Bg_bright_blue
  ; `Bg_bright_magenta
  ; `Bg_bright_cyan
  ; `Bg_bright_white
  ; `Bold
  ; `Dim
  ; `Italic
  ; `Underline
  ]
;;

let ansi_terminal_colors_message =
  let pp_self_styled style =
    Pp.tag (User_message.Style.Ansi_styles [ style ])
    @@ Pp.text (Ansi_color.Style.to_dyn style |> Dyn.to_string)
  in
  Pp.concat ~sep:Pp.newline
  @@ [ Pp.tag (User_message.Style.Ansi_styles [ `Bold ])
       @@ Pp.hbox
       @@ Pp.text "ANSI Terminal Colors and Styles:"
     ; most_ansi_colors |> Pp.enumerate ~f:pp_self_styled
     ]
;;

let pp_style_of_256 ~position color =
  let style x =
    match position with
    | `Background -> `Bg_8_bit_color x
    | `Foreground -> `Fg_8_bit_color x
  in
  Pp.tag
    (User_message.Style.Ansi_styles [ style (Ansi_color.RGB8.of_int color) ])
    (Pp.verbatim (sprintf "%3.3g" (float_of_int color)))
;;

let ansi_256_color_examples =
  let examples = [ 0; 32; 96; 160; 255 ] in
  Pp.concat ~sep:Pp.newline
  @@ [ Pp.tag (User_message.Style.Ansi_styles [ `Bold ])
       @@ Pp.hbox
       @@ Pp.text "Examples of ANSI 256-colors:"
     ; Pp.enumerate
         ~f:Fun.id
         [ Pp.concat_map examples ~f:(fun x ->
             Pp.seq (pp_style_of_256 ~position:`Background x) Pp.space)
         ; Pp.concat_map examples ~f:(fun x ->
             Pp.seq (pp_style_of_256 ~position:`Foreground x) Pp.space)
         ]
     ]
;;

let pp_style_of_24_bit ~position red green blue =
  let style x =
    match position with
    | `Background -> `Bg_24_bit_color x
    | `Foreground -> `Fg_24_bit_color x
  in
  Pp.tag
    (User_message.Style.Ansi_styles [ style (Ansi_color.RGB24.make ~red ~green ~blue) ])
  @@ Pp.space
;;

let ansi_24_color_examples =
  let examples = [ 0, 0, 0; 128, 0, 0; 0, 128, 0; 128, 128, 128 ] in
  Pp.concat ~sep:Pp.newline
  @@ [ Pp.tag (User_message.Style.Ansi_styles [ `Bold ])
       @@ Pp.hbox
       @@ Pp.text "Examples of ANSI 24-colors:"
     ; Pp.enumerate
         ~f:Fun.id
         [ Pp.concat_map examples ~f:(fun (r, g, b) ->
             Pp.seq (pp_style_of_24_bit ~position:`Background r g b) Pp.space)
         ; Pp.concat_map examples ~f:(fun (r, g, b) ->
             Pp.seq (pp_style_of_24_bit ~position:`Foreground r g b) Pp.space)
         ]
     ]
;;

let ansi_256_color_message =
  let pp_bg_style_of_256 color = pp_style_of_256 ~position:`Background color in
  let standard_and_bright_colors =
    [ Pp.tag (User_message.Style.Ansi_styles [ `Italic ])
      @@ Pp.hbox
      @@ Pp.concat
           ((Pp.text "Standard colors" :: List.init 9 ~f:(fun _ -> Pp.space))
            @ [ Pp.text "Bright colors" ])
    ; Pp.concat @@ List.init 16 ~f:(fun x -> pp_bg_style_of_256 x)
    ]
  in
  let table_216_colors =
    let width = 36 in
    let height = 216 / width in
    (Pp.tag (User_message.Style.Ansi_styles [ `Italic ])
     @@ Pp.hbox
     @@ Pp.text "216 colors")
    :: List.init height ~f:(fun row ->
      Pp.hbox
      @@ Pp.concat
      @@ List.init width ~f:(fun x -> pp_bg_style_of_256 (x + (width * row) + 16)))
  in
  let greyscale =
    [ Pp.tag (User_message.Style.Ansi_styles [ `Italic ])
      @@ Pp.hbox
      @@ Pp.text "Greyscale colors"
    ; Pp.hbox
      @@ Pp.concat
      @@ List.init 24 ~f:(fun x -> Pp.seq (pp_bg_style_of_256 (x + 232)) Pp.space)
    ]
  in
  Pp.concat ~sep:Pp.newline
  @@ [ Pp.tag (User_message.Style.Ansi_styles [ `Bold ])
       @@ Pp.hbox
       @@ Pp.text "ANSI 256-colors"
     ; [ standard_and_bright_colors; table_216_colors; greyscale ]
       |> List.map ~f:(Pp.concat ~sep:Pp.newline)
       |> Pp.enumerate ~f:Fun.id
     ]
;;

let ansi_24_bit_color_message =
  let hsv_to_rgb h s v =
    let c = v *. s in
    let h' = h /. 60. in
    let x = c *. (1. -. abs_float (mod_float h' 2. -. 1.)) in
    let m = v -. c in
    let r, g, b =
      if h' < 1.
      then c, x, 0.
      else if h' < 2.
      then x, c, 0.
      else if h' < 3.
      then 0., c, x
      else if h' < 4.
      then 0., x, c
      else if h' < 5.
      then x, 0., c
      else c, 0., x
    in
    let r = (r +. m) *. 255. |> int_of_float in
    let g = (g +. m) *. 255. |> int_of_float in
    let b = (b +. m) *. 255. |> int_of_float in
    r, g, b
  in
  let create_color_table width height =
    List.init height ~f:(fun y ->
      Pp.hbox
      @@ Pp.concat
      @@ List.init width ~f:(fun x ->
        let h = float_of_int x /. float_of_int width *. 360. in
        let s = 1. in
        let v = 1.0 -. (float_of_int y /. float_of_int height) in
        let r, g, b = hsv_to_rgb h s v in
        pp_style_of_24_bit ~position:`Background r g b))
  in
  Pp.concat ~sep:Pp.newline
  @@ [ Pp.tag (User_message.Style.Ansi_styles [ `Bold ])
       @@ Pp.hbox
       @@ Pp.text "ANSI 24-bit colors"
     ]
  @ create_color_table 110 32
;;

let document =
  [ Pp.textf "stderr supports color: %b" (Lazy.force Ansi_color.stderr_supports_color)
  ; Pp.nop
  ; user_message_styles_message
  ; Pp.nop
  ; ansi_terminal_colors_message
  ; Pp.nop
  ; ansi_256_color_examples
  ; Pp.nop
  ; ansi_24_color_examples
  ; Pp.nop
  ; ansi_256_color_message
  ; Pp.nop
  ; ansi_24_bit_color_message
  ]
;;

let main tui =
  match tui with
  | false -> Console.print document
  | true ->
    Console.Backend.(Dune_tui.backend () |> set);
    let config = Dune_config_file.Dune_config.default in
    let config =
      Dune_config_file.Dune_config.for_scheduler
        config
        None
        ~insignificant_changes:`Ignore
        ~signal_watcher:`Yes
        ~watch_exclusions:[]
    in
    Dune_engine.Scheduler.Run.go config ~on_event:(fun _ _ -> ()) ~file_watcher:No_watcher
    @@ fun () ->
    Console.print document;
    let open Fiber.O in
    Fiber.repeat_while ~init:() ~f:(fun () ->
      Dune_engine.Scheduler.sleep 0.1 >>> Fiber.return (Some ()))
;;

let term =
  let tui = Cmdliner.Arg.(value & flag & info [ "tui" ]) in
  Cmdliner.Term.(const main $ tui)
;;

let () = exit Cmdliner.Cmd.(eval (v (Cmdliner.Cmd.info "dune_console_styles") term))
