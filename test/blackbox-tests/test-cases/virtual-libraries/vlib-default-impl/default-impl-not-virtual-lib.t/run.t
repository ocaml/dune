Test that trying to specify a default implementation for a non-virtual library results
in an appropriate error message.

  $ dune build
  File "dune", line 4, characters 25-33:
  4 |  (default_implementation lib.impl))
                               ^^^^^^^^
  Error: Only virtual libraries can specify a default implementation.
  [1]

