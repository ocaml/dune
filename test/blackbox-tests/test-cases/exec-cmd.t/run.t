  $ dune clean
  $ dune exec --no-build ./foo.exe
  Error: Program "./foo.exe" isn't built yet. You need to build it first or
  remove the --no-build option.
  [1]
  $ dune exec ./foo.exe
  Foo
  $ dune exec --profile release ./foo.exe
  Foo
  $ dune exec dunetestbar --no-build
  Error: Program "dunetestbar" isn't built yet. You need to build it first or
  remove the --no-build option.
  [1]
  $ dune exec dunetestbar
  Bar

#Test the at exit bookkeeping of dune is done in the right directory
  $ dune clean

  $ mkdir test

  $ mkdir test/_build

  $ (cd test; dune exec --root .. -- dunetestbar)
  Entering directory '..'
  Leaving directory '..'
  Bar

  $ ls -a test/_build
  .
  ..
