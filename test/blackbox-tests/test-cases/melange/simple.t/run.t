Using `melange.emit` inside the same folder as the library works fine

  $ output=lib/simple
  $ dune build @mel
  $ node _build/default/$output/lib/x.js
  buy it
