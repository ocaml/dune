All builtin variables are lower cased in Dune:

  $ dune runtest --root dune-lower
  Entering directory 'dune-lower'

  $ dune runtest --root dune-upper
  Entering directory 'dune-upper'
  File "dune", line 3, characters 41-46:
  Error: Variable %{MAKE} has been renamed to %{make} since 1.0
  [1]

jbuild files retain the the old names:

  $ dune runtest --root jbuilder-upper
  Entering directory 'jbuilder-upper'

  $ dune runtest --root jbuilder-upper
  Entering directory 'jbuilder-upper'
