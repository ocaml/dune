Test melange libs flow when using `modules_without_implementation` stanza

Build js files
  $ dune build @melange
  $ node _build/default/output/b.js

writing a `melange.emit` stanza in the root `dune` file overlays JS files with
the source files

  $ find _build/default -type f -maxdepth 2 -not -path '*/.*'
  _build/default/b.ml
  _build/default/lib/lib.ml-gen
  _build/default/lib/lib.js
  _build/default/lib/a.mli
  _build/default/b.js

JS imports work correctly and the resulting JS can be executed

  $ node _build/default/b.js
  buy it
