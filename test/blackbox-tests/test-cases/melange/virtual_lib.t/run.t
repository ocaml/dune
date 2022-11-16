Virtual library with a single module are supported by melange libs

  $ dune build output/c.js
  $ node _build/default/output/c.js
  hello from melange

But if the virtual library contains some non-virtual module, compilation fails

  $ touch vlib/shared.ml
  $ dune build output/c.js
  File "output/impl/_unknown_", line 1, characters 0-0:
  Error: No rule found for output/vlib/shared.js
  File "output/impl/_unknown_", line 1, characters 0-0:
  Error: No rule found for output/vlib/vlib__.js
  [1]
