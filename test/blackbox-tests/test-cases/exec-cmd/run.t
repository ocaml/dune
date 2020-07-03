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
