  $ dune runtest --display short
  File "dune", line 65, characters 21-44:
  Warning: Directory dir-that-doesnt-exist doesn't exist.
  No rule found for jbuild
  File "dune", line 9, characters 43-47:
  Error: Variable ${^} expands to 4 values, however a single value is expected here. Please quote this atom.
  File "dune", line 16, characters 44-48:
  Error: Variable ${^} expands to 2 values, however a single value is expected here. Please quote this atom.
          diff alias runtest
  [1]
