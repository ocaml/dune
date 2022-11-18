Duplicating a field in a dune file is an error:

  $ dune build --root dune
  Entering directory 'dune'
  File "dune", line 4, characters 1-20:
  4 |  (action (echo bar)))
       ^^^^^^^^^^^^^^^^^^^
  Error: Field "action" is present too many times
  Leaving directory 'dune'
  [1]
