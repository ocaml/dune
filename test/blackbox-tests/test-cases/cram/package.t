We can attach cram tests to packages

This sub dir is needed because we are running cram inside and cram, and we
don't want our own test file to be visible.
  $ mkdir subdir
  $ cd subdir

  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > (cram enable)
  > (package (name foo))
  > (package (name bar))
  > EOF

  $ cat >dune <<EOF
  > (cram
  >  (applies_to foo)
  >  (package foo))
  > (cram
  >  (applies_to bar)
  >  (package bar))
  > EOF

  $ cat >foo.t <<EOF
  >   $ echo foo
  > EOF

  $ cat >bar.t <<EOF
  >   $ echo bar
  > EOF

  $ dune build @runtest --only-packages foo
  File "foo.t", line 1, characters 0-0:
  Error: Files _build/default/foo.t and _build/default/foo.t.corrected differ.
  [1]

  $ dune promote foo.t
  Promoting _build/default/foo.t.corrected to foo.t.


  $ dune build @runtest --only-packages bar
  File "bar.t", line 1, characters 0-0:
  Error: Files _build/default/bar.t and _build/default/bar.t.corrected differ.
  [1]

  $ dune promote bar.t
  Promoting _build/default/bar.t.corrected to bar.t.
