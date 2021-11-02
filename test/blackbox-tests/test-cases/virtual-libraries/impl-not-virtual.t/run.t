Test that trying to implement libraries that aren't virtual results in an
appropriate error message.
  $ dune build
  File "impl/dune", line 3, characters 13-16:
  3 |  (implements lib))
                   ^^^
  Error: Library "lib" is not virtual. It cannot be implemented by "impl".
  -> required by alias default in dune:1
  [1]
