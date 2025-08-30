Syntax error inside a cram command
  $ mkdir foo && cd foo
  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > EOF

  $ cat >t1.t <<EOF
  >   $ foo-bar() { true; }
  > EOF

  $ dune runtest --auto-promote 2>&1 | sed -E -e 's/.+\.sh:/$SUBTEST.sh:/' -e 's/cd.*\&\&.*.sh/cd $DIR \&\& $SUBTEST.sh/'
  File "t1.t", line 1, characters 0-0:
  Error: Files _build/default/t1.t and _build/default/t1.t.corrected differ.
  Promoting _build/default/t1.t.corrected to t1.t.

  $ cat >t1.t <<EOF
  >   $ exit 1
  >   $ echo foobar
  > EOF
  $ dune runtest --auto-promote
  File "t1.t", line 1, characters 0-0:
  Error: Files _build/default/t1.t and _build/default/t1.t.corrected differ.
  Promoting _build/default/t1.t.corrected to t1.t.
  [1]
  $ cat t1.t
    $ exit 1
    ***** UNREACHABLE *****
    $ echo foobar
    ***** UNREACHABLE *****
