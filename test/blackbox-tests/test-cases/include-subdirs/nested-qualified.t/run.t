The presence of an (include_subdirs qualified) nested under an
(include_subdirs unqualified) should work smoothly.

  $ dune exec ./foo.exe
  File "foo.ml", line 3, characters 4-18:
  3 |     Baz.Baaz.baaaz
          ^^^^^^^^^^^^^^
  Error: Unbound module Baz
  [1]

The correct output should be:

baaaz baaazo beeez quuux

cf https://github.com/ocaml/dune/issues/7630
