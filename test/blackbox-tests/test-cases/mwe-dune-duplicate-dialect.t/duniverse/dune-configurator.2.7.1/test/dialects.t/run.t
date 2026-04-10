Tests duplicate dialect definitions in vendored dune-configurator.

  $ dune build --root bad1
  Entering directory 'bad1'
  File "dune-project", lines 9-11, characters 1-74:
   9 |  (name d)
  10 |  (implementation (extension foo2))
  11 |  (interface (extension bar2)))
  Error: dialect "d" is already defined
  Leaving directory 'bad1'
  [1]
