  $ echo "(lang dune 2.3)" > dune-project
  $ cat >dune <<EOF
  > (executable (name foo) (modules foo foo-bar))
  > EOF
  $ dune exec ./foo.exe
  File "dune", line 1, characters 36-43:
  1 | (executable (name foo) (modules foo foo-bar))
                                          ^^^^^^^
  Warning: "foo-bar" is not a valid module name.
  Switch to a proper module names as this will no be allowed in future versions
  of dune.
  File "dune", line 1, characters 36-43:
  1 | (executable (name foo) (modules foo foo-bar))
                                          ^^^^^^^
  Error: Module foo-bar doesn't exist.
  [1]
