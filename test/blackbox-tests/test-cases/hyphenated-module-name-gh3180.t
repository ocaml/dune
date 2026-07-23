Tests hyphenated module name.

  $ make_dune_project 2.3
  $ cat >dune <<EOF
  > (executable (name foo) (modules foo foo-bar))
  > EOF
  $ dune exec ./foo.exe
  File "dune", line 1, characters 36-43:
  1 | (executable (name foo) (modules foo foo-bar))
                                          ^^^^^^^
  Error: Module Foo-bar doesn't exist.
  [1]
