open Stdune
module Dune_rpc = Dune_rpc_private
module Re = Dune_re

let () =
  Stdune.Path.set_root (Stdune.Path.External.of_filename_relative_to_initial_cwd ".");
  Stdune.Path.Build.set_build_dir (Stdune.Path.Outside_build_dir.of_string "_build")
;;

let test ~dir ~f main =
  let description =
    `Diagnostic (Dune_engine.Compound_user_error.make ~main ~related:[])
  in
  Dune_console.printf "---- Original ----";
  f main;
  Dune_console.printf "------- RPC ------";
  Dune_engine.Build_system_error.For_tests.make ~description ~dir ~promotion:None ()
  |> Dune_rpc_impl.Diagnostics.For_tests.diagnostic_of_error
  |> Dune_rpc_private.Diagnostic.to_user_message
  |> f
;;

let test_plain ~dir main = test main ~dir ~f:Dune_console.print_user_message

let test_dyn ~dir main =
  test main ~dir ~f:(fun x ->
    Stdune.User_message.pp x
    |> Pp.to_dyn Stdune.User_message.Style.to_dyn
    |> Dyn.to_string
    |> print_endline)
;;

let scrub output =
  Re.replace_string
    (Re.compile (Re.str Stdune.Path.(to_absolute_filename root)))
    ~by:"TEST"
    output
  |> print_endline
;;

let%expect_test "serialize and deserialize error message" =
  let dir = None in
  let message = User_error.make [ Pp.verbatim "Oh no!" ] in
  test_plain ~dir message;
  test_dyn ~dir message;
  [%expect
    {|
    ---- Original ----
    Error: Oh no!
    ------- RPC ------
    Error: Oh no!
    ---- Original ----
    Vbox
      0,
      Seq
        Box
          0,
          Concat
            Break ("", 1, ""), ("", 0, ""),
            [ Seq Tag Error, Verbatim "Error", Char :; Verbatim "Oh no!" ],
        Break ("", 0, ""), ("", 0, "")
    ------- RPC ------
    Vbox
      0,
      Seq
        Box
          0,
          Vbox
            0,
            Box
              0,
              Concat
                Break ("", 1, ""), ("", 0, ""),
                [ Seq Tag Error, Verbatim "Error", Char :; Verbatim "Oh no!" ],
        Break ("", 0, ""), ("", 0, "") |}]
;;

let%expect_test "serialize and deserialize error message with location" =
  let loc = Stdune.Loc.of_pos ("Bar", 1, 2, 3) in
  let dir = Some (Stdune.Path.of_string "/Foo") in
  let message = User_error.make ~loc [ Pp.verbatim "An error with location!" ] in
  test_plain ~dir message;
  test_dyn ~dir message;
  [%expect
    {|
    ---- Original ----
    File "Bar", line 1, characters 2-3:
    Error: An error with location!
    ------- RPC ------
    File "/Foo/Bar", line 1, characters 2-3:
    Error: An error with location!
    ---- Original ----
    Vbox
      0,
      Concat
        Nop,
        [ Seq
            Box 0, Tag Loc, Text "File \"Bar\", line 1, characters 2-3:",
            Break ("", 0, ""), ("", 0, "")
        ; Seq
            Box
              0,
              Concat
                Break ("", 1, ""), ("", 0, ""),
                [ Seq Tag Error, Verbatim "Error", Char :
                ; Verbatim "An error with location!"
                ],
            Break ("", 0, ""), ("", 0, "")
        ]
    ------- RPC ------
    Vbox
      0,
      Concat
        Nop,
        [ Seq
            Box 0, Tag Loc, Text "File \"/Foo/Bar\", line 1, characters 2-3:",
            Break ("", 0, ""), ("", 0, "")
        ; Seq
            Box
              0,
              Vbox
                0,
                Box
                  0,
                  Concat
                    Break ("", 1, ""), ("", 0, ""),
                    [ Seq Tag Error, Verbatim "Error", Char :
                    ; Verbatim "An error with location!"
                    ],
            Break ("", 0, ""), ("", 0, "")
        ] |}]
;;

let%expect_test "serialize and deserialize error with location excerpt and hint" =
  Io.String_path.write_file "foo.ml" "let x = 1\nlet y = 2\nlet z = 3\n";
  let loc = Stdune.Loc.of_pos ("foo.ml", 1, 2, 3) in
  let dir = Some (Stdune.Path.of_string ".") in
  let hints = [ Pp.verbatim "Hint 1"; Pp.verbatim "Hint 2" ] in
  let message = User_error.make ~loc ~hints [ Pp.verbatim "An error with location!" ] in
  test_plain ~dir message;
  test_dyn ~dir message;
  scrub [%expect.output];
  [%expect
    {|
    ---- Original ----
    File "foo.ml", line 1, characters 2-3:
    1 | let x = 1
          ^
    Error: An error with location!
    Hint: Hint 1
    Hint: Hint 2
    ------- RPC ------
    File "TEST/foo.ml", line 1, characters 2-3:
    1 | let x = 1
          ^
    Error: An error with location!
    Hint: Hint 1
    Hint: Hint 2
    ---- Original ----
    Vbox
      0,
      Concat
        Nop,
        [ Seq
            Box 0, Tag Loc, Text "File \"foo.ml\", line 1, characters 2-3:",
            Break ("", 0, ""), ("", 0, "")
        ; Seq
            Box
              0,
              Concat
                Break ("", 1, ""), ("", 0, ""),
                [ Seq Tag Error, Verbatim "Error", Char :
                ; Verbatim "An error with location!"
                ],
            Break ("", 0, ""), ("", 0, "")
        ; Seq
            Box
              0,
              Seq
                Seq Tag Hint, Verbatim "Hint:", Break ("", 1, ""), ("", 0, ""),
                Verbatim "Hint 1",
            Break ("", 0, ""), ("", 0, "")
        ; Seq
            Box
              0,
              Seq
                Seq Tag Hint, Verbatim "Hint:", Break ("", 1, ""), ("", 0, ""),
                Verbatim "Hint 2",
            Break ("", 0, ""), ("", 0, "")
        ]
    ------- RPC ------
    Vbox
      0,
      Concat
        Nop,
        [ Seq
            Box
              0,
              Tag
                Loc,
                Text
                  "File \"TEST/foo.ml\", line 1, characters 2-3:",
            Break ("", 0, ""), ("", 0, "")
        ; Seq
            Box
              0,
              Vbox
                0,
                Concat
                  Break ("", 0, ""), ("", 0, ""),
                  [ Box
                      0,
                      Concat
                        Break ("", 1, ""), ("", 0, ""),
                        [ Seq Tag Error, Verbatim "Error", Char :
                        ; Verbatim "An error with location!"
                        ]
                  ; Box
                      0,
                      Seq
                        Seq
                          Tag Hint, Verbatim "Hint:",
                          Break ("", 1, ""), ("", 0, ""),
                        Verbatim "Hint 1"
                  ; Box
                      0,
                      Seq
                        Seq
                          Tag Hint, Verbatim "Hint:",
                          Break ("", 1, ""), ("", 0, ""),
                        Verbatim "Hint 2"
                  ],
            Break ("", 0, ""), ("", 0, "")
        ] |}]
;;
