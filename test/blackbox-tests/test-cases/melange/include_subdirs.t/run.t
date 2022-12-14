Test that libs using `(include_subdirs unqualified) work well with
`melange.emit` stanza

Build js files
  $ output=inside/output
  $ dune build @melange
  $ node _build/default/$output/inside/c.js
  buy it
