Test default implementation for an external library

First we create an external library and implementation
  $ dune build --root lib @install
  Entering directory 'lib'
  Leaving directory 'lib'

Then we make sure that it works fine.
  $ env OCAMLPATH=lib/_build/install/default/lib dune build --root exe --debug-dependency-path
  Entering directory 'exe'
  hey
  Leaving directory 'exe'
