Test that libs using `(include_subdirs unqualified) work well with `melange.emit` stanza

Build js files
  $ dune build inside/output/c.js
  $ node _build/default/inside/output/c.js
  buy it
