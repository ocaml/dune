
  $ dune build bin/main.bc.js

  $ ls _build/default/bin
  main.bc.js
  main.bc.map
  main.ml
  main.mli

  $ dune clean
  $ dune build bin/main.bc.map
