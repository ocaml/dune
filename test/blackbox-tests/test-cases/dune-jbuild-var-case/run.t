All builtin variables are lower cased in Dune:

  $ dune runtest --root dune-lower
  Entering directory 'dune-lower'

  $ dune runtest --root dune-upper
  Entering directory 'dune-upper'
  File "dune", line 3, characters 41-46:
  Error: Uppercase variables are removed in dune files. Use: %{make}
  [1]

jbuilder retains the old names:

  $ jbuilder runtest --root jbuilder-upper
  Entering directory 'jbuilder-upper'

  $ jbuilder runtest --root jbuilder-upper
  Entering directory 'jbuilder-upper'
