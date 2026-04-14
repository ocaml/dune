This test demonstrates a bug in Dune's separation compilation of JSOO.
`std_exit.cmo` wasn't being linked in the end, so the `at_exit`
hooks didn't run and the channels weren't flushed.

Setup fixtures:

  $ echo "(lang dune 3.17)" > dune-project
  $ echo 'let () = print_string "bla"' > main.ml
  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (modes wasm))
  > EOF

Test without separate compilation:

  $ dune build --profile=release ./main.bc.wasm.js
  $ node _build/default/main.bc.wasm.js
  bla

Test with separate compilation:

  $ dune build --profile=dev ./main.bc.wasm.js
  $ node _build/default/main.bc.wasm.js
  bla

The result should be the same
