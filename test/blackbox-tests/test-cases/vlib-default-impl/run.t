Test that trying to specify a default implementation for a non-virtual library results
in an appropriate error message.
  $ dune build --root default-impl-not-virtual-lib
  Entering directory 'default-impl-not-virtual-lib'
  File "dune", line 4, characters 25-33:
  4 |  (default_implementation lib.impl))
                               ^^^^^^^^
  Error: Only virtual libraries can specify a default implementation.
  [1]

Basic sample selecting implementation according to default library.
  $ dune build --root default-impl
  Entering directory 'default-impl'
           bar alias default
  hi from lib.default

Check that ambiguity is handled correctly.
  $ dune build --root dependency-cycle
  Entering directory 'dependency-cycle'
  Error: Default implementation cycle detected between the following libraries:
     "clock"
  -> "clock_ocaml"
  -> "async_ocaml"
  -> "async"
  -> "async_c"
  -> "clock_c"
  -> "clock"
  -> "test_default"
  -> "test"
  -> required by executable bar in dune:2
  [1]

Check that default implementation data is installed in the dune package file.
  $ dune build --root dune-package
  Entering directory 'dune-package'
  File "dune", line 4, characters 25-34:
  4 |  (default_implementation a-default))
                               ^^^^^^^^^
  Error: Library "a-default" not found.
  Hint: try: dune external-lib-deps --missing --root dune-package @@default
  [1]
  $ cat dune-package/_build/install/default/lib/a/dune-package
  cat: dune-package/_build/install/default/lib/a/dune-package: No such file or directory
  [1]

Test default implementation for an external library

First we create an external library and implementation
  $ dune build --root external/lib @install
  Entering directory 'external/lib'

Then we make sure that it works fine.
  $ env OCAMLPATH=external/lib/_build/install/default/lib dune build --root external/exe --debug-dependency-path
  Entering directory 'external/exe'
           bar alias default
  hey
