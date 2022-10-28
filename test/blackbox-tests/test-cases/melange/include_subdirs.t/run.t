Test that libs using `(include_subdirs unqualified) work well with `melange.emit` stanza

Build js files
  $ dune build inside/output/app/app__B.js
  $ node _build/default/inside/output/app/app__B.js
  buy it
