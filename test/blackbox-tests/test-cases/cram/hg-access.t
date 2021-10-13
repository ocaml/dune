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

The inner call to hg shouldn't be able to access the outer hg repo:

  $ cat test.t
    $ hg root
    abort: repository requires features unknown to this Mercurial: Escaping the Dune sandbox!
    (see https://mercurial-scm.org/wiki/MissingRequirement for more information)
    [255]
