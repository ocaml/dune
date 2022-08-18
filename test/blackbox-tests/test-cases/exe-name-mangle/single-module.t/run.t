These tests show that (wrapped_executables true) addresses the problem of compilation
units of exes colliding with libraries.

Single module case. Here we technically don't need an alias module

  $ dune build
  this module is unlinkable
  this module is unlinkable

