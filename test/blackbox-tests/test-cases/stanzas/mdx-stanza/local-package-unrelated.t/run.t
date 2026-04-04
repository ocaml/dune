Dune does not fail if the `packages` are not available at evaluation time
(regression test fixed by ocaml/dune#3650)

  $ dune build -p unrelated-package

Dune fails if the `packages` are not available at execution time

  $ dune runtest -p unrelated-package
  File "dune", line 3, characters 11-14:
  3 |  (packages pkg))
                 ^^^
  Error: Package pkg does not exist
  [1]
