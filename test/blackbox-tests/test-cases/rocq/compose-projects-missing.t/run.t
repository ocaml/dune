Testing composition of theories across a dune workspace with a missing
dependency.

  $ dune build C
  File "B/dune", line 4, characters 11-12:
  4 |  (theories A))
                 ^
  Theory "A" has not been found.
  -> required by theory B in B/dune:2
  -> required by theory C in C/dune:2
  -> required by _build/default/C/.C.theory.d
  -> required by alias C/all
  -> required by alias C/default
  [1]
