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
  Error: Files _build/default/foo.t and _build/default/foo.t.corrected differ.
  [1]
  $ cat side-effect
  run
  $ dune promote
  Promoting _build/default/foo.t.corrected to foo.t.
  $ dune runtest foo.t

side-effect should only contain a single "run":

  $ cat side-effect
  run
  run
