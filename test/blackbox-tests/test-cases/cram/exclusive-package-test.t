The dir stanza should apply to cram tests:

  $ cat >dune-project <<EOF
  > (lang dune 3.22)
  > (package
  >  (name foo)
  >  (dir foo))
  > (package
  >  (name bar)
  >  (dir bar))
  > EOF

  $ mkdir foo bar

  $ cat >foo/test.t <<EOF
  >   $ echo foo
  > EOF

  $ cat >bar/test.t <<EOF
  >   $ echo foo
  > EOF

This command should only run the packages for foo:

# CR-someday rgrinberg: what happened to formatting here?!

  $ dune runtest --only-packages foo
  File "bar/test.t", line 1, characters 0-0:
  Error: Files _build/default/bar/test.t and
  _build/default/bar/test.t.corrected differ.
  File "foo/test.t", line 1, characters 0-0:
  Error: Files _build/default/foo/test.t and
  _build/default/foo/test.t.corrected differ.
  [1]
