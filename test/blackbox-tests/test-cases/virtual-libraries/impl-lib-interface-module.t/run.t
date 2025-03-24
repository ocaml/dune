Implementations may not provide a library interface module unless it is virtual.
There should be an error message that clarifies this.

  $ dune build
  File "impl/dune", lines 1-3, characters 0-41:
  1 | (library
  2 |  (name impl)
  3 |  (implements vlib))
  Error: Implementations of wrapped libraries cannot introduce new public
  modules.
  The following modules:
  - Vlib
  must all be marked as private using the (private_modules ..) field.
  [1]
