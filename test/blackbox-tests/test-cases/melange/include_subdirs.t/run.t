Test that libs using `(include_subdirs unqualified) work well with `melange.emit` stanza

Build js files
  $ dune build inside/output/melange__C.js
  $ node _build/default/inside/output/melange__C.js
  buy it
