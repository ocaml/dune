open Stdune
open Dune_engine
open Action.For_shell
module Action = Dune_engine.Action

let print x = x |> Action_to_sh.pp |> Dune_tests_common.print

let%expect_test "run" =
  Run ("my_program", Array.Immutable.of_array [| "my"; "-I"; "args" |]) |> print;
  [%expect {|
    my_program my -I args |}]
;;

(* TODO dynamic-run *)

let%expect_test "chdir" =
  Chdir ("foo", Bash "echo Hello world") |> print;
  [%expect
    {|
    mkdir -p foo;cd foo;
    bash -e -u -o pipefail -c 'echo Hello world' |}]
;;

let%expect_test "setenv" =
  Setenv ("FOO", "bar", Bash "echo Hello world") |> print;
  [%expect {|
    FOO=bar;
    bash -e -u -o pipefail -c 'echo Hello world' |}]
;;

let%expect_test "with-stdout-to" =
  Redirect_out
    (Action.Outputs.Stdout, "foo", Action.File_perm.Normal, Bash "echo Hello world")
  |> print;
  [%expect {|
    bash -e -u -o pipefail -c 'echo Hello world' > foo |}]
;;

let%expect_test "with-stderr-to" =
  Redirect_out
    (Action.Outputs.Stderr, "foo", Action.File_perm.Normal, Bash "echo Hello world")
  |> print;
  [%expect {|
    bash -e -u -o pipefail -c 'echo Hello world' 2> foo |}]
;;

let%expect_test "with-outputs-to" =
  Redirect_out
    ( Action.Outputs.Outputs
    , "foo"
    , Action.File_perm.Normal
    , Progn [ Bash "first something"; Bash "then"; Bash "echo Hello world" ] )
  |> print;
  [%expect
    {|
    {
      bash -e -u -o pipefail -c 'first something';
      bash -e -u -o pipefail -c then;
      bash -e -u -o pipefail -c 'echo Hello world';
    } &> foo |}]
;;

let%expect_test "with-outputs-to executable" =
  Redirect_out
    (Action.Outputs.Outputs, "foo", Action.File_perm.Executable, Bash "echo Hello world")
  |> print;
  [%expect
    {|
    bash -e -u -o pipefail -c 'echo Hello world' &> foo;
    chmod +x foo |}]
;;

let%expect_test "ignore stdout" =
  Ignore (Action.Outputs.Stdout, Bash "echo Hello world") |> print;
  [%expect {|
    bash -e -u -o pipefail -c 'echo Hello world' > /dev/null |}]
;;

let%expect_test "ignore stderr" =
  Ignore (Action.Outputs.Stderr, Bash "echo Hello world") |> print;
  [%expect {|
    bash -e -u -o pipefail -c 'echo Hello world' 2> /dev/null |}]
;;

let%expect_test "ignore outputs" =
  Ignore (Action.Outputs.Outputs, Bash "echo Hello world") |> print;
  [%expect {|
    bash -e -u -o pipefail -c 'echo Hello world' &> /dev/null |}]
;;

let%expect_test "with-stdin-from" =
  Redirect_in
    ( Action.Inputs.Stdin
    , "foo"
    , Bash {|
    while read line; do
      echo $line
    done
  |} )
  |> print;
  [%expect
    {|
    bash -e -u -o pipefail -c
      '
        while read line; do
          echo $line
        done
      ' < foo |}]
;;

(* TODO currently no special printing for with-accepted-exit-codes *)
let%expect_test "with-accepted-exit-codes" =
  With_accepted_exit_codes
    (Predicate_lang.of_list [ 0; 1; 123 ], Bash {|
    echo Hello world
    exit 123
  |})
  |> print;
  [%expect
    {|
    bash -e -u -o pipefail -c '
        echo Hello world
        exit 123
      ' |}]
;;

let%expect_test "progn" =
  Progn [ Bash "echo Hello"; Bash "echo world" ] |> print;
  [%expect
    {|
    bash -e -u -o pipefail -c 'echo Hello';
    bash -e -u -o pipefail -c 'echo world' |}]
;;

let%expect_test "concurrent" =
  Concurrent [ Bash "echo Hello"; Bash "echo world" ] |> print;
  [%expect
    {|
    ( bash -e -u -o pipefail -c 'echo Hello' &
      bash -e -u -o pipefail -c 'echo world' & wait ) |}]
;;

let%expect_test "echo" =
  Echo [ "Hello"; "world" ] |> print;
  [%expect {|
    echo -n Helloworld |}]
;;

let%expect_test "write-file" =
  Write_file ("foo", Action.File_perm.Normal, "Hello world") |> print;
  [%expect {|
    echo -n 'Hello world' > foo |}]
;;

let%expect_test "write-file executable" =
  Write_file ("foo", Action.File_perm.Executable, "Hello world") |> print;
  [%expect {|
      echo -n 'Hello world' > foo;
      chmod +x foo |}]
;;

let%expect_test "cat" =
  Cat [ "foo" ] |> print;
  [%expect {|
    cat foo |}]
;;

let%expect_test "cat multiple" =
  Cat [ "foo"; "bar" ] |> print;
  [%expect {|
    cat foo bar |}]
;;

let%expect_test "copy" =
  Copy ("foo", "bar") |> print;
  [%expect {|
    cp foo bar |}]
;;

let%expect_test "system" =
  System "foo bar baz" |> print;
  [%expect {|
    foo bar baz |}]
;;

let%expect_test "bash" =
  Bash "echo Hello world" |> print;
  [%expect {|
    bash -e -u -o pipefail -c 'echo Hello world' |}]
;;

let%expect_test "diff" =
  Diff
    { Action.Diff.optional = false
    ; file1 = "foo"
    ; file2 = "bar"
    ; mode = Action.Diff.Mode.Text
    }
  |> print;
  [%expect {|
    diff foo bar |}]
;;

let%expect_test "diff optional" =
  Diff
    { Action.Diff.optional = true
    ; file1 = "foo"
    ; file2 = "bar"
    ; mode = Action.Diff.Mode.Text
    }
  |> print;
  [%expect {|
    test ! -e file1 -o ! -e file2 || diff foo bar |}]
;;

let%expect_test "cmp" =
  Diff
    { Action.Diff.optional = false
    ; file1 = "foo"
    ; file2 = "bar"
    ; mode = Action.Diff.Mode.Binary
    }
  |> print;
  [%expect {|
    cmp foo bar |}]
;;

(* cmping a binary file in optional mode is not supported *)

let%expect_test "pipe-stdout-to" =
  Pipe
    ( Action.Outputs.Stdout
    , [ Bash "echo Hello world"
      ; Redirect_out
          (Action.Outputs.Stdout, "foo", Action.File_perm.Normal, Bash "echo Hello world")
      ] )
  |> print;
  [%expect
    {|
    bash -e -u -o pipefail -c 'echo Hello world'  |
      bash -e -u -o pipefail -c 'echo Hello world' > foo |}]
;;

let%expect_test "pipe-stderr-to" =
  Pipe
    ( Action.Outputs.Stderr
    , [ Bash "echo Hello world"
      ; Redirect_out
          (Action.Outputs.Stderr, "foo", Action.File_perm.Normal, Bash "echo Hello world")
      ] )
  |> print;
  [%expect
    {|
    bash -e -u -o pipefail -c 'echo Hello world'  2> >(
      bash -e -u -o pipefail -c 'echo Hello world' 2> foo  1>&2 ) |}]
;;

let%expect_test "pipe-outputs-to" =
  Pipe
    ( Action.Outputs.Outputs
    , [ Bash "echo Hello world"
      ; Redirect_out
          (Action.Outputs.Outputs, "foo", Action.File_perm.Normal, Bash "echo Hello world")
      ] )
  |> print;
  [%expect
    {|
      bash -e -u -o pipefail -c 'echo Hello world'  2>&1 |
        bash -e -u -o pipefail -c 'echo Hello world' &> foo |}]
;;
