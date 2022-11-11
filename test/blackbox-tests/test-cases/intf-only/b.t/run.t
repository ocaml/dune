Error:

  $ dune build foo.cma
  File "dune", line 3, characters 33-34:
  3 |  (modules_without_implementation x))
                                       ^
  Error: The following modules must be listed here as they don't have an
  implementation:
  - Y
  [1]
