2nd fallback form isn't allowed either

  $ dune build
  File "dune", line 2, characters 1-17:
  2 |  (fallback false)
       ^^^^^^^^^^^^^^^^
  Error: 'fallback' was renamed to '(mode fallback)' in the 1.0 version of the
  dune language
  [1]
