Test the --alias and --alias-rec flags in the dune runtest command

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > EOF

Set up cram tests that will fail so we can test running them with --alias

  $ make_failing_test() {
  >   cat >$1 <<EOF
  >   \$ echo "actual $1 output"
  >   expected $1 output
  > EOF
  > }

  $ make_failing_test test-1.t
  $ make_failing_test test-2.t
  $ make_failing_test test-3.t

  $ mkdir subdir
  $ make_failing_test subdir/test-1.t
  $ make_failing_test subdir/test-2.t

  $ test() {
  >   dune runtest "$@" 2>&1 | grep -E "^File.*\.t"
  > }

Test running a specific cram test alias with --alias

  $ test --alias test-1
  File "test-1.t", line 1, characters 0-0:

Test running multiple specific cram test aliases with --alias

  $ test --alias test-1 --alias test-2
  File "test-1.t", line 1, characters 0-0:
  File "test-2.t", line 1, characters 0-0:

Test running specific cram test alias recursively with --alias-rec

  $ test --alias-rec test-1
  File "subdir/test-1.t", line 1, characters 0-0:
  File "test-1.t", line 1, characters 0-0:

Test running multiple cram test aliases recursively

  $ test --alias-rec test-1 --alias-rec test-2
  File "subdir/test-1.t", line 1, characters 0-0:
  File "subdir/test-2.t", line 1, characters 0-0:
  File "test-1.t", line 1, characters 0-0:
  File "test-2.t", line 1, characters 0-0:

Test combining --alias and --alias-rec

  $ test --alias test-3 --alias-rec test-1
  File "subdir/test-1.t", line 1, characters 0-0:
  File "test-1.t", line 1, characters 0-0:
  File "test-3.t", line 1, characters 0-0:

Test combining path with --alias

  $ test subdir --alias test-3
  File "subdir/test-1.t", line 1, characters 0-0:
  File "subdir/test-2.t", line 1, characters 0-0:
  File "test-3.t", line 1, characters 0-0:

Test that --alias and --alias-rec work the same as build command

  $ dune build --alias test-1 2>&1 | grep -E "^File.*\.t"
  File "test-1.t", line 1, characters 0-0:

  $ dune build --alias-rec test-1 2>&1 | grep -E "^File.*\.t"
  File "subdir/test-1.t", line 1, characters 0-0:
  File "test-1.t", line 1, characters 0-0:
