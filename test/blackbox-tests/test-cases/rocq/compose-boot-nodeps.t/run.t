Testing composition with two boot libraries

  $ dune build
  File "A/dune", line 4, characters 11-12:
  4 |  (theories B)
                 ^
  Error: (boot) libraries cannot have dependencies
  [1]
