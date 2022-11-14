Using `melange.emit` inside the same folder as the library works fine

  $ output=lib/simple
  $ dune build $output/lib/melange__X.js
  $ node _build/default/$output/lib/melange__X.js
  buy it
