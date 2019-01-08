run should not access foo
  $ dune build ./run.exe 2>&1 | grep -v ocamlc
  File "run.ml", line 1, characters 34-39:
  Error: Unbound module Foo
