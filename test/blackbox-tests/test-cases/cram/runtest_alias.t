Control the default runtest alias for cram tests

  $ cat >dune-project <<EOF
  > (lang dune 3.12)
  > EOF

  $ cat >dune <<EOF
  > (cram
  >  (runtest_alias false)
  >  (alias this))
  > EOF

  $ cat >foo.t <<EOF
  >   $ echo foo
  > EOF

This shouldn't run the test

  $ dune runtest

This should run the test

  $ dune build @this
  File "foo.t", line 1, characters 0-0:
  Error: Files _build/default/foo.t and _build/default/foo.t.corrected differ.
  [1]

Now we try setting runtest alias default twice. This should be impossible:

  $ cat >dune <<EOF
  > (cram (runtest_alias false))
  > (cram (runtest_alias true))
  > EOF

  $ dune build @a
  File "dune", line 2, characters 21-25:
  2 | (cram (runtest_alias true))
                           ^^^^
  Error: enabling or disabling the runtest alias for a cram test may only be
  set once.
  It's already set for the test "foo"
  The first definition is at:
  dune:1
  [1]
