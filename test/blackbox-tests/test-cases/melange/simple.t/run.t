Using `melange.emit` inside the same folder as the library will fail

  $ dune build lib/simple/x.js
  Error: Dependency cycle between:
     _build/default/lib/simple/x.js
  [1]
