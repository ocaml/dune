Check that the error messages produced when using too many parentheses
are readable.

  $ dune build --root a
  File "dune", line 1, characters 12-72:
  These parentheses are no longer necessary with dune, please remove them.
  [1]

  $ dune build --root b
  File "dune", line 4, characters 12-17:
  These parentheses are no longer necessary with dune, please remove them.
  [1]

  $ dune build --root c
  File "dune", line 3, characters 7-14:
  These parentheses are no longer necessary with dune, please remove them.
  [1]
