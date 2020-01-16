when user tries to execute a root-defined executable without './' we suggest it

  $ dune exec foo.exe --no-build
  Error: Program "foo.exe" not found!
  Hint: did you mean ./foo.exe?
  [1]

  $ dune exec foo.exe
  Error: Program "foo.exe" not found!
  Hint: did you mean ./foo.exe?
  [1]

  $ dune exec ./foo.exe
  bar
