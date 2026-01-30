Cram tests inside (include_subdirs unqualified)

  $ make_dune_project 3.11

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
  --- bar.t/run.t
  +++ bar.t/run.t.corrected
  @@ -1 +1,2 @@
     $ echo bar
  +  bar
  File "sub/foo.t", line 1, characters 0-0:
  --- sub/foo.t
  +++ sub/foo.t.corrected
  @@ -1 +1,2 @@
     $ echo foo
  +  foo
  [1]

Previously, adding (include_subdirs unqualified) highlights two issues:

1. The file cram test in the subdirectory is no longer being run.
2. Multiple rules are being generated for the directory test.

These have now been fixed, and both cram tests work correctly with
(include_subdirs unqualified) in tandem.

  $ cat >dune <<EOF
  > (include_subdirs unqualified)
  > EOF

  $ dune runtest
  File "bar.t/run.t", line 1, characters 0-0:
  --- bar.t/run.t
  +++ bar.t/run.t.corrected
  @@ -1 +1,2 @@
     $ echo bar
  +  bar
  File "sub/foo.t", line 1, characters 0-0:
  --- sub/foo.t
  +++ sub/foo.t.corrected
  @@ -1 +1,2 @@
     $ echo foo
  +  foo
  [1]
