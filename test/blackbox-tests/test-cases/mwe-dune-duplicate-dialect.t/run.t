This test should demonstrate that cram tests inside the vendor dir have their
directories marked as data only

  $ dune build
  File "duniverse/dune-configurator.2.7.1/test/dialects.t/bad1/dune-project", line 9, characters 1-74:
   9 |  (name d)
  10 |  (implementation (extension foo2))
  11 |  (interface (extension bar2)))
  Error: dialect "d" is already defined
  [1]
