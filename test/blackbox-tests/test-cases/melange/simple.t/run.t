Using `melange.emit` inside the same folder as the library will fail

  $ dune build lib/simple/x.js
  Error: Multiple rules generated for _build/default/lib/.x.objs/x.ml.d:
  - <internal location>
  - <internal location>
  -> required by _build/default/lib/simple/x.js
  [1]
