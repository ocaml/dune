Demonstrate the files and directories listed in a cram test:

  $ cat >test.t <<EOF
  >   $ find . | sort
  > EOF

  $ dune runtest test.t
  File "test.t", line 1, characters 0-0:
  --- test.t
  +++ test.t.corrected
  @@ -1 +1,2 @@
     $ find . | sort
  +  .
  [1]

  $ dune promote
  Promoting _build/default/test.t.corrected to test.t.

  $ cat test.t
    $ find . | sort
    .

  $ ls _build/default | sort
  test.t

Now repeat the test for a test defined using a directory:

  $ mkdir foo.t
  $ cat >foo.t/run.t <<EOF
  >   $ find . | sort
  > EOF

  $ dune runtest foo.t
  File "foo.t/run.t", line 1, characters 0-0:
  --- foo.t/run.t
  +++ foo.t/run.t.corrected
  @@ -1 +1,2 @@
     $ find . | sort
  +  .
  [1]
  $ dune promote
  Promoting _build/default/foo.t/run.t.corrected to foo.t/run.t.
  $ cat foo.t/run.t
    $ find . | sort
    .
  $ ( cd _build/default/foo.t && find . | sort )
  .
  ./run.t
