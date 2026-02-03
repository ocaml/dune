This test demonstrates that we pointlessly re-run cram tests
after they're promted

  $ cat >dune-project<<EOF
  > (lang dune 3.12)
  > EOF

  $ cat >foo.t <<EOF
  >   $ echo run >> $PWD/side-effect
  >   $ echo bazy
  > EOF

  $ dune runtest foo.t
  File "foo.t", line 1, characters 0-0:
  --- foo.t
  +++ foo.t.corrected
  @@ -1,2 +1,3 @@
     $ echo run >> $TESTCASE_ROOT/side-effect
     $ echo bazy
  +  bazy
  [1]
  $ cat side-effect
  run
  $ dune promote
  Promoting _build/default/foo.t.corrected to foo.t.
  $ dune runtest foo.t

side-effect should only contain a single "run":

  $ cat side-effect
  run

However, if passing --force, we should still be able to re-run cram tests:

  $ dune runtest foo.t --force

There should be two "run"s here, however there is only one:
  $ cat side-effect
  run

