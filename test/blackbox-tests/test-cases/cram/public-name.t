Cram and public binaries with an absolute build directory

  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > (cram enable)
  > (name public-name-exe-test)
  > EOF
  $ touch public-name-exe-test.opam

  $ mkdir helper
  $ cat >helper/dune <<EOF
  > (executable (public_name helper))
  > EOF
  $ cat >helper/helper.ml <<EOF
  > print_endline "Helper launched successfully";;
  > EOF

  $ mkdir tests
  $ cat >tests/run.t <<EOF
  >   $ helper
  >   Helper launched successfully
  > EOF
  $ echo "(cram (deps %{bin:helper}))" > tests/dune

Running `dune runtest` with a relative build directory works

  $ DUNE_BUILD_DIR=./_other_build dune runtest

  $ export NEW_BUILD_DIR="$PWD/_other_build"
  $ DUNE_BUILD_DIR="$NEW_BUILD_DIR" dune runtest --auto-promote
  File "tests/run.t", line 1, characters 0-0:
  Error: Files
  $TESTCASE_ROOT/fresh/_other_build/default/tests/run.t
  and
  $TESTCASE_ROOT/fresh/_other_build/default/tests/run.t.corrected
  differ.
  Promoting
    $TESTCASE_ROOT/fresh/_other_build/default/tests/run.t.corrected
    to tests/run.t.
  [1]
