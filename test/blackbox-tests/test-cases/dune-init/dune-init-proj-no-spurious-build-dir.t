Ensure that running `dune init proj ...` doesn't create a spurious _build
directory in the current directory.

  $ dune init proj foo
  Entering directory 'foo'
  Success: initialized project component named foo
  Leaving directory 'foo'

  $ ls() {
  > command ls $@ | sort
  > }

  $ ls
  _build
  foo
  $ ls foo
  bin
  dune-project
  foo.opam
  lib
  test

  $ dune init proj foo bar
  Entering directory 'bar'
  Success: initialized project component named foo
  Leaving directory 'bar'
  $ ls
  _build
  bar
  foo
  $ ls bar
  bar
  bin
  dune-project
  foo.opam
  lib
  test

  $ mkdir baz
  $ dune init proj foo baz
  Entering directory 'baz'
  Success: initialized project component named foo
  Leaving directory 'baz'
  $ ls
  _build
  bar
  baz
  foo
  $ ls baz
  baz
  bin
  dune-project
  foo.opam
  lib
  test
