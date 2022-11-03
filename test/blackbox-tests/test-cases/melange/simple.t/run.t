Using `melange.emit` inside the same folder as the library will fail

  $ dune build lib/simple/melange__X.js
  Error: Dependency cycle between:
     _build/default/lib/simple/melange__X.js
  [1]
