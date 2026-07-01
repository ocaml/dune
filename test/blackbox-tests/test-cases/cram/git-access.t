Check that actions don't have access to the outer git repository.

  $ mkdir git
  $ cd git
  $ git init -q
  $ echo '(lang dune 3.0)' > dune-project
  $ cat >test.t <<"EOF"
  >   $ git rev-parse --show-toplevel
  > EOF

  $ dune runtest --auto-promote
  File "test.t", line 1, characters 0-0:
  --- test.t
  +++ test.t.corrected
  @@ -1 +1,3 @@
     $ git rev-parse --show-toplevel
  +  fatal: invalid gitfile format: $TESTCASE_ROOT/git/_build/.sandbox/.git
  +  [128]
  Promoting _build/default/test.t.corrected to test.t.
  [1]

The inner call to git shouldn't be able to access the outer git repo:

  $ cat test.t
    $ git rev-parse --show-toplevel
    fatal: invalid gitfile format: $TESTCASE_ROOT/git/_build/.sandbox/.git
    [128]
