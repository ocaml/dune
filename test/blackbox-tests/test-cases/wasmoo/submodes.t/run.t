Building with different combinations of submodes

Compiling to JavaScript

  $ dune build --profile js
  $ node _build/default/main.bc.js
  js_of_ocaml
  $ dune build --profile js main.bc.wasm.js
  Error: Don't know how to build main.bc.wasm.js
  [1]

Compiling to Wasm.

  $ dune build --profile wasm
  $ node _build/default/main.bc.wasm.js
  wasm_of_ocaml
  $ dune build --profile wasm main.bc.js
  Error: Don't know how to build main.bc.js
  [1]

Compiling to both JS and Wasm.

  $ dune build --profile both
  $ node _build/default/main.bc.js
  js_of_ocaml
  $ node _build/default/main.bc.wasm.js
  wasm_of_ocaml
