Test that libs using `(include_subdirs unqualified) work well with
`melange.emit` stanza

Build js files
  $ output=inside/output
  $ dune build @mel

The directory structure of the .js should mimic the directory structure of the
source:

  $ find _build/default/$output -iname "*.js" | grep -v melange | sort
  _build/default/inside/output/inside/app/b.js
  _build/default/inside/output/inside/app/lib.js
  _build/default/inside/output/inside/app/lib/a.js
  _build/default/inside/output/inside/c.js
  $ node _build/default/$output/inside/c.js
  buy it
