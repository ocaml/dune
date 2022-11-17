Virtual library with a single module are supported by melange libs

  $ dune build output/c.js
  $ node _build/default/output/c.js
  hello from melange

Implementation modules in the virtual library are supported

  $ touch vlib/shared.ml
  $ dune build output/c.js
  $ node _build/default/output/c.js
  hello from melange
