Exact path provided by the user:

  $ dune runtest --root precise-path
  Entering directory 'precise-path'
  No rule found for foo.exe
  [1]

Path that needs to be searched:

  $ dune runtest --root search-path
  Entering directory 'search-path'
  Error: Program foo-does-not-exist not found in the tree or in PATH (context: default)
  [1]
