fallback isn't allowed in dune

  $ dune build
  File "dune", line 2, characters 1-11:
  2 |  (fallback)
       ^^^^^^^^^^
  Error: 'fallback' was renamed to '(mode fallback)' in the 1.0 version of the
  dune language
  [1]
