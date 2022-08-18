These tests show that (wrapped_executables true) addresses the problem of compilation
units of exes colliding with libraries.

Multiple executables defined in the same directory

  $ dune build
