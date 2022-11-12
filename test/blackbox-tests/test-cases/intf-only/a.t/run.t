Errors:

  $ dune build foo.cma
  File "dune", line 1, characters 0-21:
  1 | (library
  2 |  (name foo))
  Error: Some modules don't have an implementation.
  You need to add the following field to this stanza:
  
    (modules_without_implementation x y)
  [1]
