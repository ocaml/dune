install/lib contains inline tests as defined with the old subsystem. Previously,
#1549 would do the same thing, but it would generate the configuration. Since
new versions of dune will generate dune-package files, we should still make sure
we understand the old files.

  $ env OCAMLPATH=install/lib dune runtest --root example
  Entering directory 'example'
