install/lib contains inline tests as defined with the old subsystem. Previously,
#1549 would do the same thing, but it would generate the configuration. Since
new versions of dune will generate dune-package files, we should still make sure
we understand the old files.

  $ env OCAMLPATH=install/lib dune runtest --root example
  Entering directory 'example'
  File "$TESTCASE_ROOT/install/lib/dune_inline_tests/dune_inline_tests.dune", line 1, characters 0-0:
  Warning: .dune files are ignored since 2.0. Reinstall the library with dune
  >= 2.0 to get rid of this warning and enable support for the subsystem this
  library provides.
  File "src/dune", line 3, characters 25-42:
  3 |   (inline_tests (backend dune_inline_tests)))
                               ^^^^^^^^^^^^^^^^^
  Error: dune_inline_tests is not an inline tests backend
  Leaving directory 'example'
  [1]
