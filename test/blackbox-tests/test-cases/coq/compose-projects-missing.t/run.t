Testing composition of theories accross a dune workspace with a missing
dependency.

  $ dune build C
  File "B/dune", line 4, characters 11-12:
  4 |  (theories A))
                 ^
  Theory A not found
  -> required by theory B in B
  -> required by theory C in C
  -> required by _build/default/C/c.v.d
  -> required by _build/default/C/.c.aux
  -> required by alias C/all
  -> required by alias C/default
  [1]
