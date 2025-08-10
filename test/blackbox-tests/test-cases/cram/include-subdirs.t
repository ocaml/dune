Cram tests inside (include_subdirs unqualified)

  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > EOF

We have a file cram test inside a subdirectory and a directory cram test. When
the (include_subdirs unqualified) is not present, both tests fail as expected.

  $ mkdir sub/
  $ cat >sub/foo.t <<EOF
  >   $ echo foo
  > EOF

  $ mkdir bar.t
  $ cat > bar.t/run.t <<EOF
  >   $ echo bar
  > EOF

  $ dune runtest
  File "bar.t/run.t", line 1, characters 0-0:
  Error: Files _build/default/bar.t/run.t and
  _build/default/bar.t/run.t.corrected differ.
  File "sub/foo.t", line 1, characters 0-0:
  Error: Files _build/default/sub/foo.t and _build/default/sub/foo.t.corrected
  differ.
  [1]

However adding (include_subdirs unqualified) highlights two issues:

1. The file cram test in the subdirectory is no longer being run.
2. Multiple rules are being generated for the directory test.

  $ cat >dune <<EOF
  > (include_subdirs unqualified)
  > EOF

  $ dune runtest
  File "bar.t/run.t", line 1, characters 0-0:
  Error: Files _build/default/bar.t/run.t and
  _build/default/bar.t/run.t.corrected differ.
  File "sub/foo.t", line 1, characters 0-0:
  Error: Files _build/default/sub/foo.t and _build/default/sub/foo.t.corrected
  differ.
  [1]
