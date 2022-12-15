Ensure that running `dune init proj ...` doesn't create a spurious _build
directory in the current directory.

  $ dune init proj foo
  Entering directory 'foo'
  Success: initialized project component named foo
  Leaving directory 'foo'
  $ ls
  foo
  $ ls foo
  _build
  bin
  dune-project
  foo.opam
  lib
  test

  $ dune init proj foo bar
  Entering directory 'bar/foo'
  Success: initialized project component named foo
  Leaving directory 'bar/foo'
  $ ls
  bar
  foo
  $ ls bar
  foo
  $ ls bar/foo
  _build
  bar
  bin
  dune-project
  foo.opam
  lib
  test

  $ mkdir baz
  $ dune init proj foo baz
  Entering directory 'baz/foo'
  Success: initialized project component named foo
  Leaving directory 'baz/foo'
  $ ls
  bar
  baz
  foo
  $ ls baz
  foo
  $ ls baz/foo
  _build
  baz
  bin
  dune-project
  foo.opam
  lib
  test
