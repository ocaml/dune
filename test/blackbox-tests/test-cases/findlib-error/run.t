We are dropping support for findlib in dune

  $ dune build --root in-dune target.txt
  Entering directory 'in-dune'
  File "dune", line 2, characters 25-37:
  Error: The findlib special variable is not supported in jbuild files, please use lib instead:
  %{lib:pkg} in dune files
  [1]

But it must still be available in jbuild files

  $ dune build --root in-jbuild target.txt
  Entering directory 'in-jbuild'
  File "jbuild", line 4, characters 23-42:
  Error: Public library "pkg" not found
  [1]
