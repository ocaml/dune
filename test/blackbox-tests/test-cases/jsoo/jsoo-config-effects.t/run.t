This tests a js_of_ocaml config of linking the same library with different,
incompatible `--effects` flags

  $ fixture=../../js-compiler-fixtures/config
  $ cp "$fixture/bin/"*.ml bin/
  $ cp "$fixture/lib/"*.ml lib/
  $ dune build bin/bin1.bc.js bin/bin2.bc.js bin/bin3.bc.js
  $ node _build/default/bin/bin1.bc.js
  Hello bin1
  Hi library1
  $ node _build/default/bin/bin2.bc.js
  Hello bin2
  Hi library1
  $ node _build/default/bin/bin3.bc.js
  Hello bin3
  Hi library1
