----------------------------------------------------------------------------------
Testsuite for https://github.com/ocaml/dune/issues/7108
`dune init` should allow dashes in `--public` names
----------------------------------------------------------------------------------

`dune init lib mirage_pair --public mirage-pair` creates public library mirage-pair

  $ dune init lib mirage_pair --public mirage-pair
  Success: initialized library component named mirage_pair
