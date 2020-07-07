These tests show that (wrapped_executables true) addresses the problem of compilation
units of exes colliding with libraries.

Single module case. Here we technically don't need an alias module

  $ dune build --root single-module
  Entering directory 'single-module'
           exe alias default
  this module is unlinkable
  this module is unlinkable

The multi module case always requires an alias.

  $ dune build --root multi-module
  Entering directory 'multi-module'
           baz alias default
  not directly usable

Multiple executables defined in the same directory

  $ dune build --root multi-exe-same-dir
  Entering directory 'multi-exe-same-dir'
