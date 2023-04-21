Testing composition with two boot libraries

  $ dune build
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in Dune 3.9
  File "A/dune", line 4, characters 11-12:
  4 |  (theories B)
                 ^
  Error: (boot) libraries cannot have dependencies
  [1]
