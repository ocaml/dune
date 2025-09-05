Cram doesn't like tests in paths

  $ mkdir "aaa bbb" && cd "aaa bbb"
  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > (cram enable)
  > EOF

  $ cat >foo.t <<EOF
  >   $ echo foo
  > EOF

  $ cat foo.t
    $ echo foo

  $ dune build @foo
  File "foo.t", line 1, characters 0-0:
  Error: Files _build/default/foo.t and _build/default/foo.t.corrected differ.
  [1]
