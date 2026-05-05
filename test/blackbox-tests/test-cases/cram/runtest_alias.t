Control the default runtest alias for cram tests

  $ make_dune_project 3.12

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
  --- foo.t
  +++ foo.t.corrected
  @@ -1 +1,2 @@
     $ echo foo
  +  foo
  [1]

Before 3.24, the runtest alias still only accepts literal booleans:

  $ make_dune_project 3.23
  $ cat >dune <<EOF
  > (cram
  >  (runtest_alias (not false)))
  > EOF
  $ dune runtest
  File "dune", line 2, characters 16-27:
  2 |  (runtest_alias (not false)))
                      ^^^^^^^^^^^
  Error: Atom expected
  [1]

The runtest alias can be controlled with a boolean expression starting from 3.24:

  $ make_dune_project 3.24
  $ rm -f foo.t.corrected
  $ cat >dune <<EOF
  > (cram
  >  (runtest_alias %{env:RUN_CRAM_RUNTEST_ALIAS=false}))
  > EOF
  $ RUN_CRAM_RUNTEST_ALIAS=false dune runtest
  $ RUN_CRAM_RUNTEST_ALIAS=true dune runtest
  File "foo.t", line 1, characters 0-0:
  --- foo.t
  +++ foo.t.corrected
  @@ -1 +1,2 @@
     $ echo foo
  +  foo
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
