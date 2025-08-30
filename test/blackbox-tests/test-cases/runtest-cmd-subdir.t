Reproduction case for https://github.com/ocaml/dune/issues/12250

  $ cat > dune-workspace <<EOF
  > (lang dune 3.19)
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.19)
  > EOF

  $ mkdir -p somelib/my/path
  $ cat > somelib/my/path/test.t <<EOF
  >   $ echo hi
  > EOF

  $ dune runtest somelib/my/path/
  File "somelib/my/path/test.t", line 1, characters 0-0:
  Error: Files _build/default/somelib/my/path/test.t and
  _build/default/somelib/my/path/test.t.corrected differ.
  [1]

dune runtest should be able to run in a subdirectory and the arguments passed
to it should be adjusted accordingly.

  $ (cd somelib && unset INSIDE_DUNE; dune runtest --diff-command=- my/path/)
  Entering directory '$TESTCASE_ROOT'
  File "somelib/my/path/test.t", line 1, characters 0-0:
  Error: Files _build/default/somelib/my/path/test.t and
  _build/default/somelib/my/path/test.t.corrected differ.
  Leaving directory '$TESTCASE_ROOT'
  [1]


