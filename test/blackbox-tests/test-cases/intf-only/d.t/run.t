Error:

  $ dune build foo.cma
  File "dune", line 3, characters 33-34:
  3 |  (modules_without_implementation x))
                                       ^
  Error: The following modules have an implementation, they cannot be listed as
  modules_without_implementation:
  - X
  [1]
