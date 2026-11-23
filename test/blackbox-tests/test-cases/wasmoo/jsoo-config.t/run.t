tests js_of_ocaml configs

  $ fixture=../../js-compiler-fixtures/config
  $ cp "$fixture/bin/"*.ml bin/
  $ cp "$fixture/lib/"*.ml lib/
  $ dune build bin/bin1.bc.wasm.js bin/bin2.bc.wasm.js bin/bin3.bc.wasm.js
  $ node _build/default/bin/bin1.bc.wasm.js
  Hello bin1
  Hi library1
  $ node _build/default/bin/bin2.bc.wasm.js
  Hello bin2
  Hi library1
  $ node _build/default/bin/bin3.bc.wasm.js
  Hello bin3
  Hi library1
