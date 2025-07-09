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
