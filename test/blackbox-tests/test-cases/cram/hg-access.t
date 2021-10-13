Check that actions don't have access to the outter hg repository.

  $ mkdir hg
  $ cd hg
  $ hg init -q
  $ echo '(lang dune 3.0)' > dune-project
  $ cat >test.t <<"EOF"
  >   $ hg root
  > EOF

  $ dune runtest --auto-promote
  File "test.t", line 1, characters 0-0:
  Error: Files _build/default/test.t and _build/default/test.t.corrected
  differ.
  Promoting _build/default/test.t.corrected to test.t.
  [1]

The inner call to hg shouldn't be able to access the outter git repo:

  $ cat test.t
    $ hg root
    $TESTCASE_ROOT/hg
