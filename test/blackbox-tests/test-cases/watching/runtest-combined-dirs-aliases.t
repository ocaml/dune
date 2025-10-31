Test that "dune runtest" with both directories and aliases fails gracefully
when another dune instance is running.

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > EOF

Set up failing cram tests for testing the combined case

  $ make_failing_test() {
  >   cat >$1 <<EOF
  >   \$ echo "actual $1 output"
  >   expected $1 output
  > EOF
  > }

  $ make_failing_test test-main.t
  $ mkdir subdir
  $ make_failing_test subdir/test-sub.t

  $ test() {
  >   dune runtest "$@" 2>&1 | grep -E "^File.*\.t"
  > }

Start dune in passive watch mode:
  $ dune build --passive-watch 2>&1 | echo > /dev/null &

  $ dune rpc ping --wait
  Server appears to be responding normally

Running tests in a directory:
  $ test subdir
  [1]

Running a test:
  $ test test-main.t
  File "test-main.t", line 1, characters 0-0:

Building an alias:
  $ test --alias test-main
  File "test-main.t", line 1, characters 0-0:

Default target:
  $ test 2>&1 | sort
  File "subdir/test-sub.t", line 1, characters 0-0:
  File "test-main.t", line 1, characters 0-0:

Both an alias and a test:
  $ test --alias test-main subdir/test-sub.t 2>&1 | sort
  File "subdir/test-sub.t", line 1, characters 0-0:
  File "test-main.t", line 1, characters 0-0:

Clean up:
  $ wait
