This tests an erroneous js_of_ocaml config with incompatible `--effects` flags

  $ dune build --verbose bin/bin1.bc.js bin/bin2.bc.js bin/bin3.bc.js
  INSERT LINK STEP ERROR HERE (it should fail but it doesn't)
  $ node _build/default/bin/bin1.bc.js
  Hello bin1
  Hi library1
  $ node _build/default/bin/bin2.bc.js
  Hello bin2
  Hi library1
  $ node _build/default/bin/bin3.bc.js
  Hello bin3
  Hi library1
