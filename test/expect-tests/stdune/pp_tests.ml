open! Stdune

let () = Dune_tests_common.init ()

let pp pp = Format.printf "%a@." Pp.render_ignore_tags pp

let enum_x_and_y =
  Pp.enumerate [ Array.make 50 "x"; Array.make 50 "y" ] ~f:(fun a ->
      Pp.concat_map (Array.to_list a) ~sep:Pp.space ~f:Pp.verbatim)

let%expect_test _ =
  pp enum_x_and_y;
  [%expect
    {|
- x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x
  x x x x x x x x x x x x
- y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y
  y y y y y y y y y y y y
|}]

let%expect_test _ =
  pp
    (Pp.enumerate
       [ Pp.enumerate [ "abc"; "def" ] ~f:Pp.text; enum_x_and_y ]
       ~f:Fun.id);
  [%expect
    {|
- - abc
  - def
- - x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x
    x x x x x x x x x x x x x
  - y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y
    y y y y y y y y y y y y y
|}]
