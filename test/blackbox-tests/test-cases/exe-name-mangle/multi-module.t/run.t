These tests show that (wrapped_executables true) addresses the problem of compilation
units of exes colliding with libraries.

The multi module case always requires an alias.

  $ dune build
  not directly usable
