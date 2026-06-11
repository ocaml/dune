The targets should only be interpreted as a single path when quoted

  $ dune build s
  File "dune", line 3, characters 25-37:
  3 |  (action (with-stdout-to "%{targets}" (echo foo))))
                               ^^^^^^^^^^^^
  Error: This action has targets in a different directory than the current one,
  this is not allowed by dune at the moment:
  - "s ./t"
  [1]
