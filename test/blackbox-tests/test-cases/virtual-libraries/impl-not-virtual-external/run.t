Test that trying to implement external libraries that aren't virtual results in
an appropriate error message.
  $ dune build
  File "dune", line 7, characters 13-30:
  7 |  (implements dune.configurator))
                   ^^^^^^^^^^^^^^^^^
  Error: Library "dune-configurator" is not virtual. It cannot be implemented
  by "foobar".
  [1]
