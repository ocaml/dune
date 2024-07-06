This test demonstrates a bug in dune's separation compilation of jsoo.
std_exit.cmo wasn't being linked in the end, which was causing at_exit hooks not
being ran and channels not being flushed.

Setup fixtures:

  $ echo "(lang dune 3.17)" > dune-project
  $ echo 'let () = print_string "bla"' > main.ml
  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (modes js)
  >  (js_of_ocaml (submodes wasm)))
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
