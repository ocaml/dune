mdx supports dependencies referencing shared libraries.

  $ dune runtest
  File "dune", lines 14-16, characters 0-56:
  14 | (mdx
  15 |  (files :standard - *.mli)
  16 |  (libraries public_lib))
  Fatal error: cannot load shared library dlltest
  Reason: dlltest.so: cannot open shared object file: No such file or directory
  [1]
