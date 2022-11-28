Using `melange.emit` inside the same folder as the library works fine

$ output=lib/simple/lib/simple
  $ dune build @melange
  $ node _build/default/$output/x.js
  buy it
