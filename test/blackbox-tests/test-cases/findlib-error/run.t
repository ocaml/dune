We are dropping support for findlib in dune

  $ dune build --root in-dune target.txt
  Entering directory 'in-dune'
  File "dune", line 2, characters 25-37:
  Error: Variable %{findlib:pkg} has been renamed to %{lib:pkg} since 1.0
  [1]

But it must still be available in jbuild files

  $ dune build --root in-jbuild target.txt
  Entering directory 'in-jbuild'
  File "jbuild", line 4, characters 23-42:
  Error: Public library "pkg" not found
  [1]
