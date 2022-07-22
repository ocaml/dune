Implementations cannot introduce new modules to the library's interface
  $ dune build
  File "impl/dune", line 1, characters 0-44:
  1 | (library
  2 |  (name foo_impl)
  3 |  (implements foo))
  Error: Implementations of wrapped libraries cannot introduce new public
  modules.
  The following modules:
  - Baz
  must all be marked as private using the (private_modules ..) field.
  [1]
