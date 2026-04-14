Syntax error inside a cram command
  $ mkdir foo && cd foo
  $ make_dune_project 3.0

  $ cat >t1.t <<EOF
  >   $ if then fi
  > EOF

  $ dune runtest --auto-promote --diff-command -
  File "t1.t", line 1, characters 0-0:
  Error: Files _build/default/t1.t and _build/default/t1.t.corrected differ.
  Promoting _build/default/t1.t.corrected to t1.t.
  [1]

  $ cat >t1.t <<EOF
  >   $ exit 1
  >   $ echo foobar
  > EOF
  $ dune runtest --auto-promote
  File "t1.t", line 1, characters 0-0:
  --- t1.t
  +++ t1.t.corrected
  @@ -1,2 +1,4 @@
     $ exit 1
  +  ***** UNREACHABLE *****
     $ echo foobar
  +  ***** UNREACHABLE *****
  Promoting _build/default/t1.t.corrected to t1.t.
  [1]
  $ cat t1.t
    $ exit 1
    ***** UNREACHABLE *****
    $ echo foobar
    ***** UNREACHABLE *****
