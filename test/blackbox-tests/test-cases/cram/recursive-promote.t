Demonstrate that cram tests that add new commands are correctly executed:

  $  make_dune_project 3.17

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
  --- test.t
  +++ test.t.corrected
  @@ -1 +1,2 @@
     $ echo "$ echo foo"
  +  $ echo foo
  Promoting _build/default/test.t.corrected to test.t.
    $ echo "$ echo foo"
    $ echo foo

  $ runTest
  File "test.t", line 1, characters 0-0:
  --- test.t
  +++ test.t.corrected
  @@ -1,2 +1,4 @@
     $ echo "$ echo foo"
     $ echo foo
  +  $ echo foo
  +  foo
  Promoting _build/default/test.t.corrected to test.t.
    $ echo "$ echo foo"
    $ echo foo
    $ echo foo
    foo

  $ runTest
  File "test.t", line 1, characters 0-0:
  --- test.t
  +++ test.t.corrected
  @@ -2,3 +2,5 @@
     $ echo foo
     $ echo foo
     foo
  +  $ echo foo
  +  foo
  Promoting _build/default/test.t.corrected to test.t.
    $ echo "$ echo foo"
    $ echo foo
    $ echo foo
    foo
    $ echo foo
    foo
