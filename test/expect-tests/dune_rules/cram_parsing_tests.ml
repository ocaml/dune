open Stdune
open Dune_rules.For_tests.Cram_exec.For_tests

let () = Dune_tests_common.init ()

let with_chdir dir f =
  let old_cwd = Sys.getcwd () in
  Sys.chdir (Path.to_string dir);
  Exn.protect ~f ~finally:(fun () -> Sys.chdir old_cwd)
;;

let test content =
  Temp.with_temp_dir
    ~parent_dir:(Path.of_string (Sys.getcwd ()))
    ~prefix:"cram_test"
    ~suffix:""
    ~f:(fun temp_dir_result ->
      let temp_dir = Result.ok_exn temp_dir_result in
      with_chdir temp_dir
      @@ fun () ->
      let test_file = Path.relative temp_dir "test.t" in
      Io.write_file test_file content;
      let lexbuf = Lexing.from_string ~with_positions:true content in
      Stdlib.Lexing.set_filename lexbuf (Path.basename test_file);
      cram_stanzas lexbuf
      |> Pp.concat_map ~sep:Pp.cut ~f:(fun (loc, block) ->
        [ Loc.pp loc; dyn_of_block block |> Dyn.pp; Loc.to_dyn loc |> Dyn.pp ]
        |> Pp.concat ~sep:Pp.cut
        |> Pp.vbox)
      |> Pp.map_tags ~f:(fun _ -> User_message.Style.Loc)
      |> List.singleton
      |> Dune_console.print)
;;

let%expect_test "simple single line command" =
  test "  $ echo 'Hello world'";
  [%expect
    {|
    File "test.t", line 1, characters 2-22:
    1 |   $ echo 'Hello world'
          ^^^^^^^^^^^^^^^^^^^^

    Command [ "echo 'Hello world'" ]
    { pos_fname = "test.t"
    ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 2 }
    ; stop = { pos_lnum = 1; pos_bol = 0; pos_cnum = 22 }
    }
    |}];
  test "  $ echo 'Hello world'\n";
  [%expect
    {|
    File "test.t", line 1, characters 2-22:
    1 |   $ echo 'Hello world'
          ^^^^^^^^^^^^^^^^^^^^

    Command [ "echo 'Hello world'" ]
    { pos_fname = "test.t"
    ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 2 }
    ; stop = { pos_lnum = 1; pos_bol = 0; pos_cnum = 22 }
    }
    |}]
;;

let%expect_test "command with single continuation" =
  test "  $ echo 'Hello'\n  > echo 'World!'";
  [%expect
    {|
    File "test.t", lines 1-2, characters 2-34:
    1 |   $ echo 'Hello'
    2 |   > echo 'World!'

    Command [ "echo 'Hello'"; "echo 'World!'" ]
    { pos_fname = "test.t"
    ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 2 }
    ; stop = { pos_lnum = 2; pos_bol = 17; pos_cnum = 34 }
    }
    |}];
  test "  $ echo 'Hello'\n  > echo 'World!'\n";
  [%expect
    {|
    File "test.t", lines 1-2, characters 2-34:
    1 |   $ echo 'Hello'
    2 |   > echo 'World!'

    Command [ "echo 'Hello'"; "echo 'World!'" ]
    { pos_fname = "test.t"
    ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 2 }
    ; stop = { pos_lnum = 2; pos_bol = 17; pos_cnum = 34 }
    }
    |}]
;;

let%expect_test "command with empty continutation" =
  test "  $ echo 'Hello..'\n  >";
  [%expect
    {|
    File "test.t", lines 1-2, characters 2-22:
    1 |   $ echo 'Hello..'
    2 |   >

    Command [ "echo 'Hello..'"; "" ]
    { pos_fname = "test.t"
    ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 2 }
    ; stop = { pos_lnum = 2; pos_bol = 19; pos_cnum = 22 }
    }
    |}];
  test "  $ echo 'Hello..'\n  >\n";
  [%expect
    {|
    File "test.t", lines 1-2, characters 2-22:
    1 |   $ echo 'Hello..'
    2 |   >

    Command [ "echo 'Hello..'"; "" ]
    { pos_fname = "test.t"
    ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 2 }
    ; stop = { pos_lnum = 2; pos_bol = 19; pos_cnum = 22 }
    }
    |}]
;;

(* Output lines are not parsed. They are skipped. When we run a cram test, we
   re-output the results and reassemble the cram test. *)
let%expect_test "command followed by output lines" =
  test "  $ echo 'Hi'\n  Hi";
  [%expect
    {|
    File "test.t", line 1, characters 2-13:
    1 |   $ echo 'Hi'
          ^^^^^^^^^^^

    Command [ "echo 'Hi'" ]
    { pos_fname = "test.t"
    ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 2 }
    ; stop = { pos_lnum = 1; pos_bol = 0; pos_cnum = 13 }
    }
    |}];
  test "  $ echo 'Hi'\n  Hi\n";
  [%expect
    {|
    File "test.t", line 1, characters 2-13:
    1 |   $ echo 'Hi'
          ^^^^^^^^^^^

    Command [ "echo 'Hi'" ]
    { pos_fname = "test.t"
    ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 2 }
    ; stop = { pos_lnum = 1; pos_bol = 0; pos_cnum = 13 }
    }
    |}]
;;

let%expect_test "empty output following a command" =
  test "  $ echo 'Hi'\n  ";
  [%expect
    {|
    File "test.t", line 1, characters 2-13:
    1 |   $ echo 'Hi'
          ^^^^^^^^^^^

    Command [ "echo 'Hi'" ]
    { pos_fname = "test.t"
    ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 2 }
    ; stop = { pos_lnum = 1; pos_bol = 0; pos_cnum = 13 }
    }
    |}];
  test "  $ echo 'Hi'\n  \n";
  [%expect
    {|
    File "test.t", line 1, characters 2-13:
    1 |   $ echo 'Hi'
          ^^^^^^^^^^^

    Command [ "echo 'Hi'" ]
    { pos_fname = "test.t"
    ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 2 }
    ; stop = { pos_lnum = 1; pos_bol = 0; pos_cnum = 13 }
    }
    |}]
;;

let%expect_test "loose output lines only" =
  test "  orphan\n  output";
  [%expect {| |}];
  test "  orphan\n  output\n";
  [%expect {| |}]
;;

let%expect_test "single-space and text is a comment line" =
  test " foo";
  [%expect
    {|
    File "test.t", line 1, characters 0-4:
    1 |  foo
        ^^^^

    Comment [ " foo" ]
    { pos_fname = "test.t"
    ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
    ; stop = { pos_lnum = 1; pos_bol = 0; pos_cnum = 4 }
    }
    |}];
  test " foo\n";
  [%expect
    {|
    File "test.t", line 1, characters 0-4:
    1 |  foo
        ^^^^

    Comment [ " foo" ]
    { pos_fname = "test.t"
    ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
    ; stop = { pos_lnum = 1; pos_bol = 0; pos_cnum = 4 }
    }
    |}]
;;

let%expect_test "line with a single space only" =
  test " ";
  [%expect
    {|
    File "test.t", line 1, characters 0-1:
    1 |
        ^

    Comment [ " " ]
    { pos_fname = "test.t"
    ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
    ; stop = { pos_lnum = 1; pos_bol = 0; pos_cnum = 1 }
    }
    |}];
  test " \n";
  [%expect
    {|
    File "test.t", line 1, characters 0-1:
    1 |
        ^

    Comment [ " " ]
    { pos_fname = "test.t"
    ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
    ; stop = { pos_lnum = 1; pos_bol = 0; pos_cnum = 1 }
    }
    |}]
;;

let%expect_test "empty becomes empty" =
  test "";
  [%expect {| |}]
;;

let%expect_test "blank line becomes empty-string comment" =
  test "\n";
  [%expect
    {|
    File "test.t", line 1, characters 0-0:

    Comment [ "" ]
    { pos_fname = "test.t"
    ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
    ; stop = { pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
    }
    |}]
;;

let%expect_test "group consecutive comment lines" =
  test "First line\nSecond line";
  [%expect
    {|
    File "test.t", lines 1-2, characters 0-22:
    1 | First line
    2 | Second line

    Comment [ "First line"; "Second line" ]
    { pos_fname = "test.t"
    ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
    ; stop = { pos_lnum = 2; pos_bol = 11; pos_cnum = 22 }
    }
    |}];
  test "First line\nSecond line\n";
  [%expect
    {|
    File "test.t", lines 1-2, characters 0-22:
    1 | First line
    2 | Second line

    Comment [ "First line"; "Second line" ]
    { pos_fname = "test.t"
    ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
    ; stop = { pos_lnum = 2; pos_bol = 11; pos_cnum = 22 }
    }
    |}]
;;

let%expect_test "comment then command" =
  test "Intro text\n  $ cmd";
  [%expect
    {|
    File "test.t", line 1, characters 0-10:
    1 | Intro text
        ^^^^^^^^^^

    Comment [ "Intro text" ]
    { pos_fname = "test.t"
    ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
    ; stop = { pos_lnum = 1; pos_bol = 0; pos_cnum = 10 }
    }
    File "test.t", line 2, characters 2-7:
    2 |   $ cmd
          ^^^^^

    Command [ "cmd" ]
    { pos_fname = "test.t"
    ; start = { pos_lnum = 2; pos_bol = 11; pos_cnum = 13 }
    ; stop = { pos_lnum = 2; pos_bol = 11; pos_cnum = 18 }
    }
    |}];
  test "Intro text\n  $ cmd\n";
  [%expect
    {|
    File "test.t", line 1, characters 0-10:
    1 | Intro text
        ^^^^^^^^^^

    Comment [ "Intro text" ]
    { pos_fname = "test.t"
    ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
    ; stop = { pos_lnum = 1; pos_bol = 0; pos_cnum = 10 }
    }
    File "test.t", line 2, characters 2-7:
    2 |   $ cmd
          ^^^^^

    Command [ "cmd" ]
    { pos_fname = "test.t"
    ; start = { pos_lnum = 2; pos_bol = 11; pos_cnum = 13 }
    ; stop = { pos_lnum = 2; pos_bol = 11; pos_cnum = 18 }
    }
    |}]
;;

let%expect_test "orphan output lines before command" =
  test "  ignored\n  also ignored\n  $ real\n";
  [%expect
    {|
    File "test.t", line 3, characters 2-8:
    3 |   $ real
          ^^^^^^

    Command [ "real" ]
    { pos_fname = "test.t"
    ; start = { pos_lnum = 3; pos_bol = 25; pos_cnum = 27 }
    ; stop = { pos_lnum = 3; pos_bol = 25; pos_cnum = 33 }
    }
    |}]
;;

let%expect_test "mixed document" =
  test "Doc line A\nDoc line B\n  $ echo one\n  one\n\n  $ echo two\n  two\n";
  [%expect
    {|
    File "test.t", lines 1-2, characters 0-21:
    1 | Doc line A
    2 | Doc line B

    Comment [ "Doc line A"; "Doc line B" ]
    { pos_fname = "test.t"
    ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
    ; stop = { pos_lnum = 2; pos_bol = 11; pos_cnum = 21 }
    }
    File "test.t", line 3, characters 2-12:
    3 |   $ echo one
          ^^^^^^^^^^

    Command [ "echo one" ]
    { pos_fname = "test.t"
    ; start = { pos_lnum = 3; pos_bol = 22; pos_cnum = 24 }
    ; stop = { pos_lnum = 3; pos_bol = 22; pos_cnum = 34 }
    }
    File "test.t", line 5, characters 0-0:

    Comment [ "" ]
    { pos_fname = "test.t"
    ; start = { pos_lnum = 5; pos_bol = 41; pos_cnum = 41 }
    ; stop = { pos_lnum = 5; pos_bol = 41; pos_cnum = 41 }
    }
    File "test.t", line 6, characters 2-12:
    6 |   $ echo two
          ^^^^^^^^^^

    Command [ "echo two" ]
    { pos_fname = "test.t"
    ; start = { pos_lnum = 6; pos_bol = 42; pos_cnum = 44 }
    ; stop = { pos_lnum = 6; pos_bol = 42; pos_cnum = 54 }
    }
    |}]
;;

let%expect_test "comments separated by new line" =
  test "Comment 1\n\nComment 2\n";
  [%expect
    {|
    File "test.t", lines 1-3, characters 0-20:
    1 | Comment 1
    2 |
    3 | Comment 2

    Comment [ "Comment 1"; ""; "Comment 2" ]
    { pos_fname = "test.t"
    ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
    ; stop = { pos_lnum = 3; pos_bol = 11; pos_cnum = 20 }
    }
    |}]
;;

let%expect_test "stray command continuation is a comment" =
  test "  > stray\n  $ cmd";
  [%expect
    {|
    File "test.t", line 1, characters 0-9:
    1 |   > stray
        ^^^^^^^^^

    Comment [ "  > stray" ]
    { pos_fname = "test.t"
    ; start = { pos_lnum = 1; pos_bol = 0; pos_cnum = 0 }
    ; stop = { pos_lnum = 1; pos_bol = 0; pos_cnum = 9 }
    }
    File "test.t", line 2, characters 2-7:
    2 |   $ cmd
          ^^^^^

    Command [ "cmd" ]
    { pos_fname = "test.t"
    ; start = { pos_lnum = 2; pos_bol = 10; pos_cnum = 12 }
    ; stop = { pos_lnum = 2; pos_bol = 10; pos_cnum = 17 }
    }
    |}]
;;
