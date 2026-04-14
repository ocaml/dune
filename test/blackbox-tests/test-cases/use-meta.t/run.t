Tests use_meta with installed libraries.

  $ dune build @install && dune exec -- ocamlfind opt -package foobarlib -linkpkg main.ml -o main.exe && ./main.exe
  foobarlib
