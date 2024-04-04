Extension registered twice

  $ dune build
  File "dune-project", lines 9-11, characters 1-74:
   9 |  (name d2)
  10 |  (implementation (extension foo))
  11 |  (interface (extension bar2)))
  Error: extension "foo" is already registered by dialect "d"
  [1]

