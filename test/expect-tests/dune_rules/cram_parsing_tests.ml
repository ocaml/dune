open Stdune
open Dune_rules.For_tests.Cram_exec.For_tests

let () = Dune_tests_common.init ()

let test content =
  let lexbuf = Lexing.from_string ~with_positions:true content in
  Stdlib.Lexing.set_filename lexbuf "test";
  cram_stanzas lexbuf
  |> Dyn.list (Dyn.pair Loc.to_dyn dyn_of_block)
  |> Dyn.pp
  |> List.singleton
  |> Dune_console.print
;;

let%expect_test "simple single line command" =
  test "  $ echo 'Hello world'";
  [%expect
    {|
    [ ({ pos_fname = "test"
       ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 2 }
       ; stop = { pos_lnum = 1; pos_bol = 0; pos_cnum = 22 }
       },
       Command [ "echo 'Hello world'" ])
    ]
    |}];
  test "  $ echo 'Hello world'\n";
  [%expect
    {|
    [ ({ pos_fname = "test"
       ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 2 }
       ; stop = { pos_lnum = 1; pos_bol = 0; pos_cnum = 22 }
       },
       Command [ "echo 'Hello world'" ])
    ]
    |}]
;;

let%expect_test "command with single continuation" =
  test "  $ echo 'Hello'\n  > echo 'World!'";
  [%expect
    {|
    [ ({ pos_fname = "test"
       ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 2 }
       ; stop = { pos_lnum = 2; pos_bol = 17; pos_cnum = 34 }
       },
       Command [ "echo 'Hello'"; "echo 'World!'" ])
    ]
    |}];
  test "  $ echo 'Hello'\n  > echo 'World!'\n";
  [%expect
    {|
    [ ({ pos_fname = "test"
       ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 2 }
       ; stop = { pos_lnum = 2; pos_bol = 17; pos_cnum = 34 }
       },
       Command [ "echo 'Hello'"; "echo 'World!'" ])
    ]
    |}]
;;

let%expect_test "command with empty continutation" =
  test "  $ echo 'Hello..'\n  >";
  [%expect
    {|
    [ ({ pos_fname = "test"
       ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 2 }
       ; stop = { pos_lnum = 2; pos_bol = 19; pos_cnum = 22 }
       },
       Command [ "echo 'Hello..'"; "" ])
    ]
    |}];
  test "  $ echo 'Hello..'\n  >\n";
  [%expect
    {|
    [ ({ pos_fname = "test"
       ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 2 }
       ; stop = { pos_lnum = 2; pos_bol = 19; pos_cnum = 22 }
       },
       Command [ "echo 'Hello..'"; "" ])
    ]
    |}]
;;

(* Output lines are not parsed. They are skipped. When we run a cram test, we
   re-output the results and reassemble the cram test. *)
let%expect_test "command followed by output lines" =
  test "  $ echo 'Hi'\n  Hi";
  [%expect
    {|
    [ ({ pos_fname = "test"
       ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 2 }
       ; stop = { pos_lnum = 1; pos_bol = 0; pos_cnum = 13 }
       },
       Command [ "echo 'Hi'" ])
    ]
    |}];
  test "  $ echo 'Hi'\n  Hi\n";
  [%expect
    {|
    [ ({ pos_fname = "test"
       ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 2 }
       ; stop = { pos_lnum = 1; pos_bol = 0; pos_cnum = 13 }
       },
       Command [ "echo 'Hi'" ])
    ]
    |}]
;;

let%expect_test "empty output following a command" =
  test "  $ echo 'Hi'\n  ";
  [%expect
    {|
    [ ({ pos_fname = "test"
       ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 2 }
       ; stop = { pos_lnum = 1; pos_bol = 0; pos_cnum = 13 }
       },
       Command [ "echo 'Hi'" ])
    ]
    |}];
  test "  $ echo 'Hi'\n  \n";
  [%expect
    {|
    [ ({ pos_fname = "test"
       ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 2 }
       ; stop = { pos_lnum = 1; pos_bol = 0; pos_cnum = 13 }
       },
       Command [ "echo 'Hi'" ])
    ]
    |}]
;;

let%expect_test "loose output lines only" =
  test "  orphan\n  output";
  [%expect {| [] |}];
  test "  orphan\n  output\n";
  [%expect {| [] |}]
;;

let%expect_test "single-space and text is a comment line" =
  test " foo";
  [%expect
    {|
    [ ({ pos_fname = "test"
       ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
       ; stop = { pos_lnum = 1; pos_bol = 0; pos_cnum = 4 }
       },
       Comment [ " foo" ])
    ]
    |}];
  test " foo\n";
  [%expect
    {|
    [ ({ pos_fname = "test"
       ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
       ; stop = { pos_lnum = 1; pos_bol = 0; pos_cnum = 4 }
       },
       Comment [ " foo" ])
    ]
    |}]
;;

let%expect_test "line with a single space only" =
  test " ";
  [%expect
    {|
    [ ({ pos_fname = "test"
       ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
       ; stop = { pos_lnum = 1; pos_bol = 0; pos_cnum = 1 }
       },
       Comment [ " " ])
    ]
    |}];
  test " \n";
  [%expect
    {|
    [ ({ pos_fname = "test"
       ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
       ; stop = { pos_lnum = 1; pos_bol = 0; pos_cnum = 1 }
       },
       Comment [ " " ])
    ]
    |}]
;;

let%expect_test "empty becomes empty" =
  test "";
  [%expect {| [] |}]
;;

let%expect_test "blank line becomes empty-string comment" =
  test "\n";
  [%expect
    {|
    [ ({ pos_fname = "test"
       ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
       ; stop = { pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
       },
       Comment [ "" ])
    ]
    |}]
;;

let%expect_test "group consecutive comment lines" =
  test "First line\nSecond line";
  [%expect
    {|
    [ ({ pos_fname = "test"
       ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
       ; stop = { pos_lnum = 2; pos_bol = 11; pos_cnum = 22 }
       },
       Comment [ "First line"; "Second line" ])
    ]
    |}];
  test "First line\nSecond line\n";
  [%expect
    {|
    [ ({ pos_fname = "test"
       ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
       ; stop = { pos_lnum = 2; pos_bol = 11; pos_cnum = 22 }
       },
       Comment [ "First line"; "Second line" ])
    ]
    |}]
;;

let%expect_test "comment then command" =
  test "Intro text\n  $ cmd";
  [%expect
    {|
    [ ({ pos_fname = "test"
       ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
       ; stop = { pos_lnum = 1; pos_bol = 0; pos_cnum = 10 }
       },
       Comment [ "Intro text" ])
    ; ({ pos_fname = "test"
       ; start = { pos_lnum = 2; pos_bol = 11; pos_cnum = 13 }
       ; stop = { pos_lnum = 2; pos_bol = 11; pos_cnum = 18 }
       },
       Command [ "cmd" ])
    ]
    |}];
  test "Intro text\n  $ cmd\n";
  [%expect
    {|
    [ ({ pos_fname = "test"
       ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
       ; stop = { pos_lnum = 1; pos_bol = 0; pos_cnum = 10 }
       },
       Comment [ "Intro text" ])
    ; ({ pos_fname = "test"
       ; start = { pos_lnum = 2; pos_bol = 11; pos_cnum = 13 }
       ; stop = { pos_lnum = 2; pos_bol = 11; pos_cnum = 18 }
       },
       Command [ "cmd" ])
    ]
    |}]
;;

let%expect_test "orphan output lines before command" =
  test "  ignored\n  also ignored\n  $ real\n";
  [%expect
    {|
    [ ({ pos_fname = "test"
       ; start = { pos_lnum = 3; pos_bol = 25; pos_cnum = 27 }
       ; stop = { pos_lnum = 3; pos_bol = 25; pos_cnum = 33 }
       },
       Command [ "real" ])
    ]
    |}]
;;

let%expect_test "mixed document" =
  test "Doc line A\nDoc line B\n  $ echo one\n  one\n\n  $ echo two\n  two\n";
  [%expect
    {|
    [ ({ pos_fname = "test"
       ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
       ; stop = { pos_lnum = 2; pos_bol = 11; pos_cnum = 21 }
       },
       Comment [ "Doc line A"; "Doc line B" ])
    ; ({ pos_fname = "test"
       ; start = { pos_lnum = 3; pos_bol = 22; pos_cnum = 24 }
       ; stop = { pos_lnum = 3; pos_bol = 22; pos_cnum = 34 }
       },
       Command [ "echo one" ])
    ; ({ pos_fname = "test"
       ; start = { pos_lnum = 5; pos_bol = 41; pos_cnum = 41 }
       ; stop = { pos_lnum = 5; pos_bol = 41; pos_cnum = 41 }
       },
       Comment [ "" ])
    ; ({ pos_fname = "test"
       ; start = { pos_lnum = 6; pos_bol = 42; pos_cnum = 44 }
       ; stop = { pos_lnum = 6; pos_bol = 42; pos_cnum = 54 }
       },
       Command [ "echo two" ])
    ]
    |}]
;;

let%expect_test "comments separated by new line" =
  test "Comment 1\n\nComment 2\n";
  [%expect
    {|
    [ ({ pos_fname = "test"
       ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
       ; stop = { pos_lnum = 3; pos_bol = 11; pos_cnum = 20 }
       },
       Comment [ "Comment 1"; ""; "Comment 2" ])
    ]
    |}]
;;

let%expect_test "stray command continuation is a comment" =
  test "  > stray\n  $ cmd";
  [%expect
    {|
    [ ({ pos_fname = "test"
       ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
       ; stop = { pos_lnum = 1; pos_bol = 0; pos_cnum = 9 }
       },
       Comment [ "  > stray" ])
    ; ({ pos_fname = "test"
       ; start = { pos_lnum = 2; pos_bol = 10; pos_cnum = 12 }
       ; stop = { pos_lnum = 2; pos_bol = 10; pos_cnum = 17 }
       },
       Command [ "cmd" ])
    ]
    |}]
;;
