open Stdune
open Dune_rules.For_tests.Cram_exec.For_tests

let () = Dune_tests_common.init ()

let test content =
  let lexbuf = Lexing.from_string ~with_positions:true content in
  Stdlib.Lexing.set_filename lexbuf "test";
  cram_stanzas lexbuf
  |> Dyn.list dyn_of_block
  |> Dyn.pp
  |> List.singleton
  |> Dune_console.print
;;

let%expect_test "simple single line command" =
  test "  $ echo 'Hello world'";
  [%expect
    {|
    [ Command
        ({ pos_fname = "test"
         ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 2 }
         ; stop = { pos_lnum = 1; pos_bol = 0; pos_cnum = 22 }
         },
         [ "echo 'Hello world'" ])
    ]
    |}];
  test "  $ echo 'Hello world'\n";
  [%expect
    {|
    [ Command
        ({ pos_fname = "test"
         ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 2 }
         ; stop = { pos_lnum = 1; pos_bol = 0; pos_cnum = 22 }
         },
         [ "echo 'Hello world'" ])
    ]
    |}]
;;

let%expect_test "command with single continuation" =
  test "  $ echo 'Hello'\n  > echo 'World!'";
  [%expect
    {|
    [ Command
        ({ pos_fname = "test"
         ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 2 }
         ; stop = { pos_lnum = 2; pos_bol = 17; pos_cnum = 34 }
         },
         [ "echo 'Hello'"; "echo 'World!'" ])
    ]
    |}];
  test "  $ echo 'Hello'\n  > echo 'World!'\n";
  [%expect
    {|
    [ Command
        ({ pos_fname = "test"
         ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 2 }
         ; stop = { pos_lnum = 2; pos_bol = 17; pos_cnum = 34 }
         },
         [ "echo 'Hello'"; "echo 'World!'" ])
    ]
    |}]
;;

let%expect_test "command with empty continutation" =
  test "  $ echo 'Hello..'\n  >";
  [%expect
    {|
    [ Command
        ({ pos_fname = "test"
         ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 2 }
         ; stop = { pos_lnum = 2; pos_bol = 19; pos_cnum = 22 }
         },
         [ "echo 'Hello..'"; "" ])
    ]
    |}];
  test "  $ echo 'Hello..'\n  >\n";
  [%expect
    {|
    [ Command
        ({ pos_fname = "test"
         ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 2 }
         ; stop = { pos_lnum = 2; pos_bol = 19; pos_cnum = 22 }
         },
         [ "echo 'Hello..'"; "" ])
    ]
    |}]
;;

(* Output lines are not parsed. They are skipped. When we run a cram test, we
   re-output the results and reassemble the cram test. *)
let%expect_test "command followed by output lines" =
  test "  $ echo 'Hi'\n  Hi";
  [%expect
    {|
    [ Command
        ({ pos_fname = "test"
         ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 2 }
         ; stop = { pos_lnum = 1; pos_bol = 0; pos_cnum = 13 }
         },
         [ "echo 'Hi'" ])
    ]
    |}];
  test "  $ echo 'Hi'\n  Hi\n";
  [%expect
    {|
    [ Command
        ({ pos_fname = "test"
         ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 2 }
         ; stop = { pos_lnum = 1; pos_bol = 0; pos_cnum = 13 }
         },
         [ "echo 'Hi'" ])
    ]
    |}]
;;

let%expect_test "empty output following a command" =
  test "  $ echo 'Hi'\n  ";
  [%expect
    {|
    [ Command
        ({ pos_fname = "test"
         ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 2 }
         ; stop = { pos_lnum = 1; pos_bol = 0; pos_cnum = 13 }
         },
         [ "echo 'Hi'" ])
    ]
    |}];
  test "  $ echo 'Hi'\n  \n";
  [%expect
    {|
    [ Command
        ({ pos_fname = "test"
         ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 2 }
         ; stop = { pos_lnum = 1; pos_bol = 0; pos_cnum = 13 }
         },
         [ "echo 'Hi'" ])
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
  [%expect {| [ Comment [ " foo" ] ] |}];
  test " foo\n";
  [%expect {| [ Comment [ " foo" ] ] |}]
;;

let%expect_test "line with a single space only" =
  test " ";
  [%expect {| [ Comment [ " " ] ] |}];
  test " \n";
  [%expect {| [ Comment [ " " ] ] |}]
;;

let%expect_test "empty becomes empty" =
  test "";
  [%expect {| [] |}]
;;

let%expect_test "blank line becomes empty-string comment" =
  test "\n";
  [%expect {| [ Comment [ "" ] ] |}]
;;

let%expect_test "group consecutive comment lines" =
  test "First line\nSecond line";
  [%expect {| [ Comment [ "First line"; "Second line" ] ] |}];
  test "First line\nSecond line\n";
  [%expect {| [ Comment [ "First line"; "Second line" ] ] |}]
;;

let%expect_test "comment then command" =
  test "Intro text\n  $ cmd";
  [%expect
    {|
    [ Comment [ "Intro text" ]
    ; Command
        ({ pos_fname = "test"
         ; start = { pos_lnum = 2; pos_bol = 11; pos_cnum = 13 }
         ; stop = { pos_lnum = 2; pos_bol = 11; pos_cnum = 18 }
         },
         [ "cmd" ])
    ]
    |}];
  test "Intro text\n  $ cmd\n";
  [%expect
    {|
    [ Comment [ "Intro text" ]
    ; Command
        ({ pos_fname = "test"
         ; start = { pos_lnum = 2; pos_bol = 11; pos_cnum = 13 }
         ; stop = { pos_lnum = 2; pos_bol = 11; pos_cnum = 18 }
         },
         [ "cmd" ])
    ]
    |}]
;;

let%expect_test "orphan output lines before command" =
  test "  ignored\n  also ignored\n  $ real\n";
  [%expect
    {|
    [ Command
        ({ pos_fname = "test"
         ; start = { pos_lnum = 3; pos_bol = 25; pos_cnum = 27 }
         ; stop = { pos_lnum = 3; pos_bol = 25; pos_cnum = 33 }
         },
         [ "real" ])
    ]
    |}]
;;

let%expect_test "mixed document" =
  test "Doc line A\nDoc line B\n  $ echo one\n  one\n\n  $ echo two\n  two\n";
  [%expect
    {|
    [ Comment [ "Doc line A"; "Doc line B" ]
    ; Command
        ({ pos_fname = "test"
         ; start = { pos_lnum = 3; pos_bol = 22; pos_cnum = 24 }
         ; stop = { pos_lnum = 3; pos_bol = 22; pos_cnum = 34 }
         },
         [ "echo one" ])
    ; Comment [ "" ]
    ; Command
        ({ pos_fname = "test"
         ; start = { pos_lnum = 6; pos_bol = 42; pos_cnum = 44 }
         ; stop = { pos_lnum = 6; pos_bol = 42; pos_cnum = 54 }
         },
         [ "echo two" ])
    ]
    |}]
;;

let%expect_test "comments separated by new line" =
  test "Comment 1\n\nComment 2\n";
  [%expect {| [ Comment [ "Comment 1"; ""; "Comment 2" ] ] |}]
;;

let%expect_test "stray command continuation is a comment" =
  test "  > stray\n  $ cmd";
  [%expect
    {|
    [ Comment [ "  > stray" ]
    ; Command
        ({ pos_fname = "test"
         ; start = { pos_lnum = 2; pos_bol = 10; pos_cnum = 12 }
         ; stop = { pos_lnum = 2; pos_bol = 10; pos_cnum = 17 }
         },
         [ "cmd" ])
    ]
    |}]
;;
