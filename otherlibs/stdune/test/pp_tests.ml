open Stdune

let test_truncate length pp =
  let pp = Pp.truncate length pp in
  Format.printf "%s" (Pp.to_dyn Dyn.opaque pp |> Dyn.to_string)
;;

let test_truncate_and_print length pp =
  let pp = Pp.truncate length pp in
  Format.printf "%a" Pp.to_fmt pp
;;

let%expect_test "truncation at the boundary" =
  test_truncate_and_print 5 (Pp.text "X");
  [%expect {|
    X |}];
  test_truncate_and_print 5 (Pp.text "XX");
  [%expect {|
    XX |}];
  test_truncate_and_print 5 (Pp.text "XXX");
  [%expect {|
    XXX |}];
  test_truncate_and_print 5 (Pp.text "XXXX");
  [%expect {|
    XXXX |}];
  test_truncate_and_print 5 (Pp.text "XXXXX");
  [%expect {|
    XX... |}];
  test_truncate_and_print 5 (Pp.text "XXXXXX");
  [%expect {|
    XX... |}]
;;

let%expect_test "truncating text" =
  Pp.text "This is some text." |> test_truncate 10;
  [%expect {|
    Seq Text "This is",Text "..." |}]
;;

let%expect_test "truncating verbatim" =
  Pp.verbatim "This is some text." |> test_truncate 10;
  [%expect {|
    Seq Verbatim "This is",Text "..." |}]
;;

let%expect_test "truncating seq" =
  let seq = Pp.seq (Pp.text "First part.") (Pp.text "Second part.") in
  test_truncate 5 seq;
  [%expect {|
    Seq Seq Text "Fi",Nop,Text "..." |}];
  test_truncate 11 seq;
  [%expect {|
    Seq Seq Text "First pa",Nop,Text "..." |}];
  test_truncate 15 seq;
  [%expect {|
    Seq Seq Text "First part.",Text "S",Text "..." |}];
  test_truncate 23 seq;
  [%expect {|
    Seq Seq Text "First part.",Text "Second pa",Text "..." |}]
;;

let%expect_test "truncating concat" =
  let concat =
    Pp.concat [ Pp.text "First part."; Pp.text "Second part."; Pp.text "Third part." ]
  in
  (* untruncated *)
  Format.printf "%s" (Pp.to_dyn Dyn.opaque concat |> Dyn.to_string);
  [%expect
    {| Concat Nop,[ Text "First part."; Text "Second part."; Text "Third part." ] |}];
  (* truncating the first part *)
  test_truncate 5 concat;
  [%expect {|
    Seq Concat Nop,[ Text "Fi"; Nop; Nop ],Text "..." |}];
  (* truncating the second part *)
  test_truncate 20 concat;
  [%expect {|
    Seq Concat Nop,[ Text "First part."; Text "Second"; Nop ],Text "..." |}];
  (* truncating the third part *)
  test_truncate 30 concat;
  [%expect
    {|
    Seq
      Concat Nop,[ Text "First part."; Text "Second part."; Text "Thir" ],
      Text
        "..." |}];
  (* truncating to the end *)
  test_truncate 33 concat;
  [%expect
    {|
      Seq
        Concat Nop,[ Text "First part."; Text "Second part."; Text "Third p" ],
        Text
          "..." |}]
;;

let%expect_test "truncation and ascii sequences" =
  let example =
    "This is a \027[38;2;255;0;0mblue\027[39m string with \027[38;2;0;255;0mred\027[39m \
     and \027[38;2;0;0;255mgreen\027[39m together with strings of a \
     \027[48;2;255;0;0mblue blackground\027[49m and \027[48;2;0;255;0mred \
     background\027[49m and \027[48;2;0;0;255mgreen background\027[49m"
  in
  Ansi_color.parse example |> Pp.truncate 80 |> Ansi_color.print;
  [%expect
    {|
This is a blue string with red and green together with strings of a blue blac...
|}]
;;

let%expect_test "truncation should preserve tags" =
  test_truncate 5 (Pp.tag "tag" (Pp.text "This is some text."));
  [%expect {|
    Seq Tag <opaque>,Text "Th",Text "..." |}]
;;

let%expect_test "truncation of newlines" =
  let example =
    Pp.hbox (Pp.concat [ Pp.text "This is"; Pp.newline; Pp.text "some text." ])
  in
  let length = 10 in
  test_truncate length example;
  [%expect {| Seq Hbox Concat Nop,[ Text "This is"; Nop; Nop ],Text "..." |}];
  test_truncate_and_print length example;
  [%expect {|
    This is... |}]
;;

let%expect_test "truncation of vbox" =
  let example = Pp.vbox (Pp.concat [ Pp.text "This is some text." ]) in
  let length = 10 in
  test_truncate length example;
  [%expect {| Seq Hbox Text "This is",Text "..." |}];
  test_truncate_and_print length example;
  [%expect {|
    This is... |}]
;;

let%expect_test "truncation of hbox" =
  let example = Pp.hbox (Pp.concat [ Pp.text "This is some text." ]) in
  let length = 10 in
  test_truncate length example;
  [%expect {| Seq Hbox Text "This is",Text "..." |}];
  test_truncate_and_print length example;
  [%expect {|
    This is... |}]
;;

let%expect_test "truncation of hvbox" =
  (* these should be turned into hboxes *)
  let example = Pp.hvbox (Pp.concat [ Pp.text "This is some text." ]) in
  let length = 10 in
  test_truncate length example;
  [%expect {| Seq Hbox Text "This is",Text "..." |}];
  test_truncate_and_print length example;
  [%expect {|
    This is... |}]
;;

let%expect_test "truncation of hovbox" =
  (* these should be turned into hboxes *)
  let example = Pp.hovbox (Pp.concat [ Pp.text "This is some text." ]) in
  let length = 10 in
  test_truncate length example;
  [%expect {| Seq Hbox Text "This is",Text "..." |}];
  test_truncate_and_print length example;
  [%expect {|
    This is... |}]
;;

let%expect_test "truncation of box" =
  let example = Pp.box (Pp.concat [ Pp.text "This is some text." ]) in
  let length = 10 in
  test_truncate length example;
  [%expect {| Seq Hbox Text "This is",Text "..." |}];
  test_truncate_and_print length example;
  [%expect {|
    This is... |}]
;;

let%expect_test "truncatation less than 3" =
  (* Should not emit more than 3 characters *)
  test_truncate_and_print 2 (Pp.text "XXXX");
  [%expect {|
    ... |}];
  test_truncate_and_print 2 (Pp.text "X");
  [%expect {|
    X |}]
;;
