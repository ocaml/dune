Demonstrate that cram tests that add new commands are correctly executed:

  $  cat >dune-project <<EOF
  > (lang dune 3.17)
  > EOF

  $ cat >test.t <<EOF
  >   $ echo "$ echo foo"
  > EOF

  $ runTest() {
  > dune runtest test.t
  > dune promote
  > cat test.t
  > }

  $ runTest
  File "test.t", line 1, characters 0-0:
  Error: Files _build/default/test.t and _build/default/test.t.corrected
  differ.
  Promoting _build/default/test.t.corrected to test.t.
    $ echo "$ echo foo"
    $ echo foo

  $ runTest
  File "test.t", line 1, characters 0-0:
  Error: Files _build/default/test.t and _build/default/test.t.corrected
  differ.
  Promoting _build/default/test.t.corrected to test.t.
    $ echo "$ echo foo"
    $ echo foo
    $ echo foo
    foo

  $ runTest
  File "test.t", line 1, characters 0-0:
  Error: Files _build/default/test.t and _build/default/test.t.corrected
  differ.
  Promoting _build/default/test.t.corrected to test.t.
    $ echo "$ echo foo"
    $ echo foo
    $ echo foo
    foo
    $ echo foo
    foo
