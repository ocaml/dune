Exact path provided by the user:

  $ dune runtest --root precise-path
  Entering directory 'precise-path'
  File "dune", line 1, characters 0-49:
  Error: No rule found for foo.exe
  [1]

Path that needs to be searched:

  $ dune runtest --root search-path
  Entering directory 'search-path'
  File "dune", line 3, characters 14-32:
  Error: Error: Program foo-does-not-exist not found in the tree or in PATH (context: default)
  [1]

Path in deps field of alias stanza

  $ dune runtest --root alias-deps-field
  Entering directory 'alias-deps-field'
  File "dune", line 1, characters 0-38:
  Error: No rule found for foobar
  [1]
