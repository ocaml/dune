Using `melange.emit` inside the same folder as the library works fine

  $ dune build @melange
  $ node _build/default/app/output/app/x.js 2>&1 | grep "Cannot find"
  Error: Cannot find module '../lib/y.js'
