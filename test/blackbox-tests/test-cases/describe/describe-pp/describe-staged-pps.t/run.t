It should fail with a message that `describe pp` doesn't support `staged_pps`.

  $ dune describe pp src/main.ml
  File "src/dune", line 4, characters 2-25:
  4 |   (staged_pps ppx_suffix)))
        ^^^^^^^^^^^^^^^^^^^^^^^
  Error: staged_pps are not supported.
  [1]
