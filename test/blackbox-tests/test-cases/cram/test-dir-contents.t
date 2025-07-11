Demonstrate the files and directories listed in a cram test:

  $ cat >test.t <<EOF
  >   $ find . | sort
  > EOF

  $ dune runtest test.t
  File "test.t", line 1, characters 0-0:
  Error: Files _build/default/test.t and _build/default/test.t.corrected
  differ.
  [1]

  $ dune promote
  Promoting _build/default/test.t.corrected to test.t.

  $ cat test.t
    $ find . | sort
    .
    ./.cram.test.t

  $ ls _build/default | sort
  test.t

Now repeat the test for a test defined using a directory:

  $ mkdir foo.t
  $ touch foo.t/run.t
  $ cat >test.t <<EOF
  >   $ find . | sort
  > EOF

  $ dune runtest foo.t
  $ dune promote
  $ cat foo.t/run.t
  $ ( cd _build/default/foo.t && find . | sort )
  .
  ./run.t
