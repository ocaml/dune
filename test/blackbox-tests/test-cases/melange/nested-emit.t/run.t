Test the private libs flow when using `melange.emit` stanza

  $ output=lib/emit-output/lib/emit-output/

Build js files

  $ dune build @melange-emit
  $ node _build/default/$output/c.js
  buy it
