open Stdune

let dyn_of_pp tag pp =
  let rec conv =
    let open Dyn in
    function
    | Pp.Ast.Nop -> variant "Nop" []
    | Seq (x, y) -> variant "Seq" [ conv x; conv y ]
    | Concat (x, y) -> variant "Concat" [ conv x; list conv y ]
    | Box (i, x) -> variant "Box" [ int i; conv x ]
    | Vbox (i, x) -> variant "Vbox" [ int i; conv x ]
    | Hbox x -> variant "Hbox" [ conv x ]
    | Hvbox (i, x) -> variant "Hvbox" [ int i; conv x ]
    | Hovbox (i, x) -> variant "Hovbox" [ int i; conv x ]
    | Verbatim s -> variant "Verbatim" [ string s ]
    | Char c -> variant "Char" [ char c ]
    | Break (x, y) ->
      let f = triple string int string in
      variant "Break" [ f x; f y ]
    | Newline -> variant "Newline" []
    | Tag (ta, t) -> variant "Tag" [ tag ta; conv t ]
    | Text s -> variant "Text" [ string s ]
  in
  conv
    (match Pp.to_ast pp with
     | Ok s -> s
     | Error () -> assert false)
;;

let%expect_test "reproduce #2664" =
  (* https://github.com/ocaml/dune/issues/2664 *)
  let b = Buffer.create 100 in
  let f s = Buffer.add_string b ("\027[34m" ^ s ^ "\027[39m") in
  for i = 1 to 20 do
    f (string_of_int i)
  done;
  let string_with_ansi_colors = Buffer.contents b in
  let pp = Ansi_color.parse string_with_ansi_colors in
  let ansi_colors_from_pp =
    let b = Buffer.create 16 in
    let ppf = Format.formatter_of_buffer b in
    Staged.unstage (Ansi_color.make_printer (lazy true) ppf) pp;
    Buffer.contents b
  in
  printfn "Original : %S" string_with_ansi_colors;
  printfn "From PP  : %S" ansi_colors_from_pp;
  [%expect
    {|
    Original : "\027[34m1\027[39m\027[34m2\027[39m\027[34m3\027[39m\027[34m4\027[39m\027[34m5\027[39m\027[34m6\027[39m\027[34m7\027[39m\027[34m8\027[39m\027[34m9\027[39m\027[34m10\027[39m\027[34m11\027[39m\027[34m12\027[39m\027[34m13\027[39m\027[34m14\027[39m\027[34m15\027[39m\027[34m16\027[39m\027[34m17\027[39m\027[34m18\027[39m\027[34m19\027[39m\027[34m20\027[39m"
    From PP  : "\027[34m1\027[0m\027[34m2\027[0m\027[34m3\027[0m\027[34m4\027[0m\027[34m5\027[0m\027[34m6\027[0m\027[34m7\027[0m\027[34m8\027[0m\027[34m9\027[0m\027[34m10\027[0m\027[34m11\027[0m\027[34m12\027[0m\027[34m13\027[0m\027[34m14\027[0m\027[34m15\027[0m\027[34m16\027[0m\027[34m17\027[0m\027[34m18\027[0m\027[34m19\027[0m\027[34m20\027[0m" |}];
  let pp = dyn_of_pp (Dyn.list Ansi_color.Style.to_dyn) pp |> Dyn.pp in
  Format.printf "%a@.%!" Pp.to_fmt pp;
  [%expect
    {|
    Vbox
      0,
      Seq
        Seq
          Seq
            Seq
              Seq
                Seq
                  Seq
                    Seq
                      Seq
                        Seq
                          Seq
                            Seq
                              Seq
                                Seq
                                  Seq
                                    Seq
                                      Seq
                                        Seq
                                          Seq
                                            Seq
                                              Nop,
                                              Tag [ Fg_blue ], Verbatim "1",
                                            Tag [ Fg_blue ], Verbatim "2",
                                          Tag [ Fg_blue ], Verbatim "3",
                                        Tag [ Fg_blue ], Verbatim "4",
                                      Tag [ Fg_blue ], Verbatim "5",
                                    Tag [ Fg_blue ], Verbatim "6",
                                  Tag [ Fg_blue ], Verbatim "7",
                                Tag [ Fg_blue ], Verbatim "8",
                              Tag [ Fg_blue ], Verbatim "9",
                            Tag [ Fg_blue ], Verbatim "10",
                          Tag [ Fg_blue ], Verbatim "11",
                        Tag [ Fg_blue ], Verbatim "12",
                      Tag [ Fg_blue ], Verbatim "13",
                    Tag [ Fg_blue ], Verbatim "14",
                  Tag [ Fg_blue ], Verbatim "15",
                Tag [ Fg_blue ], Verbatim "16",
              Tag [ Fg_blue ], Verbatim "17",
            Tag [ Fg_blue ], Verbatim "18",
          Tag [ Fg_blue ], Verbatim "19",
        Tag [ Fg_blue ], Verbatim "20" |}]
;;

let%expect_test "Ansi_color.strip" =
  print_string
    (String.concat
       ~sep:"\n"
       (List.map
          ~f:Ansi_color.strip
          [ "\027[34mthe lazy fox\027[39m jumps over the brown dog\027[0m"
          ; "the lazy fox \027[34mjumps over\027[39m the brown dog\027[0m"
          ; "\027[34mthe lazy fox\027[39m jumps over \027[0mthe brown dog"
          ; "\027[34mthe lazy fox \027[39mjumps over\027[0thebrown dog"
          ]));
  [%expect
    {|
the lazy fox jumps over the brown dog
the lazy fox jumps over the brown dog
the lazy fox jumps over the brown dog
the lazy fox jumps over|}]
;;

let%expect_test "parse fg and bg colors" =
  let example =
    "This is a \027[34mblue\027[39m string with \027[31mred\027[39m and \
     \027[32mgreen\027[39m together with strings of a \027[44mblue blackground\027[49m \
     and \027[41mred background\027[49m and \027[42mgreen background\027[49m"
  in
  Ansi_color.parse example
  |> dyn_of_pp (Dyn.list Ansi_color.Style.to_dyn)
  |> Dyn.pp
  |> Format.printf "%a@.%!" Pp.to_fmt;
  [%expect
    {|
Vbox
  0,
  Seq
    Seq
      Seq
        Seq
          Seq
            Seq
              Seq
                Seq
                  Seq
                    Seq
                      Seq
                        Seq Nop, Verbatim "This is a ",
                        Tag [ Fg_blue ], Verbatim "blue",
                      Verbatim " string with ",
                    Tag [ Fg_red ], Verbatim "red",
                  Verbatim " and ",
                Tag [ Fg_green ], Verbatim "green",
              Verbatim " together with strings of a ",
            Tag [ Bg_blue ], Verbatim "blue blackground",
          Verbatim " and ",
        Tag [ Bg_red ], Verbatim "red background",
      Verbatim " and ",
    Tag [ Bg_green ], Verbatim "green background" |}]
;;

let%expect_test "parse multiple fg and bg colors" =
  let example =
    "This text is \027[34;41mblue string with a red background\027[0m and \
     \027[32;44mgreen string with a blue background\027[0m"
  in
  Ansi_color.parse example
  |> dyn_of_pp (Dyn.list Ansi_color.Style.to_dyn)
  |> Dyn.pp
  |> Format.printf "%a@.%!" Pp.to_fmt;
  [%expect
    {|
Vbox
  0,
  Seq
    Seq
      Seq
        Seq Nop, Verbatim "This text is ",
        Tag [ Fg_blue; Bg_red ], Verbatim "blue string with a red background",
      Verbatim " and ",
    Tag [ Fg_green; Bg_blue ], Verbatim "green string with a blue background" |}]
;;

let%expect_test "fg default overrides" =
  let example =
    "This text has a \027[34mblue foreground\027[39m but here it becomes the default \
     foreground,\027[34;39m even together with another foreground modifier."
  in
  Ansi_color.parse example
  |> dyn_of_pp (Dyn.list Ansi_color.Style.to_dyn)
  |> Dyn.pp
  |> Format.printf "%a@.%!" Pp.to_fmt;
  [%expect
    {|
  Vbox
    0,
    Seq
      Seq
        Seq
          Seq Nop, Verbatim "This text has a ",
          Tag [ Fg_blue ], Verbatim "blue foreground",
        Verbatim " but here it becomes the default foreground,",
      Verbatim " even together with another foreground modifier." |}]
;;

let%expect_test "bg default overrides" =
  let example =
    "This text has a \027[44mblue background\027[49m but here it becomes the default \
     background,\027[44;49m even together with another background modifier."
  in
  Ansi_color.parse example
  |> dyn_of_pp (Dyn.list Ansi_color.Style.to_dyn)
  |> Dyn.pp
  |> Format.printf "%a@.%!" Pp.to_fmt;
  [%expect
    {|
Vbox
  0,
  Seq
    Seq
      Seq
        Seq Nop, Verbatim "This text has a ",
        Tag [ Bg_blue ], Verbatim "blue background",
      Verbatim " but here it becomes the default background,",
    Verbatim " even together with another background modifier." |}]
;;

let%expect_test "parse 8-bit colors" =
  let example =
    "This is a \027[38;5;33mblue\027[39m string with \027[38;5;196mred\027[39m and \
     \027[38;5;46mgreen\027[39m together with strings of a \027[48;5;33mblue \
     blackground\027[49m and \027[48;5;196mred background\027[49m and \027[48;5;46mgreen \
     background\027[49m"
  in
  Ansi_color.parse example
  |> dyn_of_pp (Dyn.list Ansi_color.Style.to_dyn)
  |> Dyn.pp
  |> Format.printf "%a@.%!" Pp.to_fmt;
  [%expect
    {|
Vbox
  0,
  Seq
    Seq
      Seq
        Seq
          Seq
            Seq
              Seq
                Seq
                  Seq
                    Seq
                      Seq
                        Seq Nop, Verbatim "This is a ",
                        Tag [ Fg_8_bit_color 33 ], Verbatim "blue",
                      Verbatim " string with ",
                    Tag [ Fg_8_bit_color 196 ], Verbatim "red",
                  Verbatim " and ",
                Tag [ Fg_8_bit_color 46 ], Verbatim "green",
              Verbatim " together with strings of a ",
            Tag [ Bg_8_bit_color 33 ], Verbatim "blue blackground",
          Verbatim " and ",
        Tag [ Bg_8_bit_color 196 ], Verbatim "red background",
      Verbatim " and ",
    Tag [ Bg_8_bit_color 46 ], Verbatim "green background" |}]
;;

let%expect_test "parse 24-bit colors" =
  let example =
    "This is a \027[38;2;255;0;0mblue\027[39m string with \027[38;2;0;255;0mred\027[39m \
     and \027[38;2;0;0;255mgreen\027[39m together with strings of a \
     \027[48;2;255;0;0mblue blackground\027[49m and \027[48;2;0;255;0mred \
     background\027[49m and \027[48;2;0;0;255mgreen background\027[49m"
  in
  Ansi_color.parse example
  |> dyn_of_pp (Dyn.list Ansi_color.Style.to_dyn)
  |> Dyn.pp
  |> Format.printf "%a@.%!" Pp.to_fmt;
  [%expect
    {|
    Vbox
      0,
      Seq
        Seq
          Seq
            Seq
              Seq
                Seq
                  Seq
                    Seq
                      Seq
                        Seq
                          Seq
                            Seq Nop, Verbatim "This is a ",
                            Tag
                              [ Fg_24_bit_color [ 255; 0; 0 ] ],
                              Verbatim "blue",
                          Verbatim " string with ",
                        Tag [ Fg_24_bit_color [ 0; 255; 0 ] ], Verbatim "red",
                      Verbatim " and ",
                    Tag [ Fg_24_bit_color [ 0; 0; 255 ] ], Verbatim "green",
                  Verbatim " together with strings of a ",
                Tag
                  [ Bg_24_bit_color [ 255; 0; 0 ] ],
                  Verbatim "blue blackground",
              Verbatim " and ",
            Tag [ Bg_24_bit_color [ 0; 255; 0 ] ], Verbatim "red background",
          Verbatim " and ",
        Tag [ Bg_24_bit_color [ 0; 0; 255 ] ], Verbatim "green background"
|}]
;;
