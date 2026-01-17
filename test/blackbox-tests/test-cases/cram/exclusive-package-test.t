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

  $ tests=("foo/file.t" "bar/file.t" "foo/dir.t/run.t" "bar/dir.t/run.t")

  $ for file in "${tests[@]}"; do
  >   mkdir -p $(dirname $file)
  >   echo "  $ echo foo" > $file
  > done

This command should only run the packages for foo:

# CR-someday rgrinberg: what happened to formatting here?!

  $ dune runtest --only-packages foo
  File "bar/dir.t/run.t", line 1, characters 0-0:
  Error: Files _build/default/bar/dir.t/run.t and
  _build/default/bar/dir.t/run.t.corrected differ.
  File "bar/file.t", line 1, characters 0-0:
  Error: Files _build/default/bar/file.t and
  _build/default/bar/file.t.corrected differ.
  File "foo/dir.t/run.t", line 1, characters 0-0:
  Error: Files _build/default/foo/dir.t/run.t and
  _build/default/foo/dir.t/run.t.corrected differ.
  File "foo/file.t", line 1, characters 0-0:
  Error: Files _build/default/foo/file.t and
  _build/default/foo/file.t.corrected differ.
  [1]
