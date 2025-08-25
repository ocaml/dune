Test that trying to use virtual_modules with an incompatible kind reports a user
error

  $ echo "(lang dune 3.20)" > dune-project

  $ touch empty.mli
  $ cat >dune <<EOF
  > (library
  >  (name invalid_lib)
  >  (kind parameter)
  >  (virtual_modules empty))
  > EOF
  $ dune build
  File "dune", lines 1-4, characters 0-72:
  1 | (library
  2 |  (name invalid_lib)
  3 |  (kind parameter)
  4 |  (virtual_modules empty))
  Error: Only virtual libraries can have 'virtual_modules'
  but this library has 'virtual_modules' and is specified as kind 'parameter'.
  Hint: Remove either the 'kind' or 'virtual_modules' fields
  [1]
