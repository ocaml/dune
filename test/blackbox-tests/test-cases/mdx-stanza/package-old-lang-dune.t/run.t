(package) needs a recent (lang dune):

  $ dune runtest --only-packages b
  File "dune", line 3, characters 1-12:
  3 |  (package a))
       ^^^^^^^^^^^
  Error: 'package' is only available since version 2.9 of the dune language.
  Please update your dune-project file to have (lang dune 2.9).
  [1]
