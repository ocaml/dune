Test file unmangling with melange.emit that depends on a library, that depends on another library

  $ dune build dist/entry_module.js
  $ node _build/default/dist/entry_module.js
  1
