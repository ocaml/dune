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
  File "foo/dir.t/run.t", line 1, characters 0-0:
  --- foo/dir.t/run.t
  +++ foo/dir.t/run.t.corrected
  @@ -1 +1,2 @@
     $ echo foo
  +  foo
  File "foo/file.t", line 1, characters 0-0:
  --- foo/file.t
  +++ foo/file.t.corrected
  @@ -1 +1,2 @@
     $ echo foo
  +  foo
  [1]

The following cram stanza should be an error:

  $ cat >foo/dune <<EOF
  > (cram (package bar))
  > EOF

  $ dune runtest
  File "foo/dune", line 1, characters 15-18:
  1 | (cram (package bar))
                     ^^^
  Error: Package bar may not be defined here
  The only package that can be defined in this directory is foo because the
  directory of this stanza is exclusive to this package
  [1]

Redundant, but acceptable:

  $ cat >foo/dune <<EOF
  > (cram (package foo))
  > EOF

  $ dune runtest --diff-command -
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
