when user tries to execute a current-dir-defined
executable without './' we suggest it

  $ dune exec foo.exe --no-build
  Error: Program 'foo.exe' not found!
  Hint: did you mean ./foo.exe?
  [1]

  $ dune exec foo.exe
  Error: Program 'foo.exe' not found!
  Hint: did you mean ./foo.exe?
  [1]

  $ dune exec ./foo.exe
  foo

  $ (cd foo && dune exec bar.exe --no-build)
  Error: Program 'bar.exe' not found!
  Hint: did you mean ./bar.exe?
  [1]

  $ (cd foo && dune exec bar.exe)
  Error: Program 'bar.exe' not found!
  Hint: did you mean ./bar.exe?
  [1]

  $ (cd foo && dune exec ./bar.exe)
  foo/bar
