env vars interpreted in various fields, such as flags

  $ dune build --force
  var visible from dune: -principal
  DUNE_FOO: -principal

global vars are overridden

  $ DUNE_FOO=blarg dune build --force
  var visible from dune: -principal
  DUNE_FOO: -principal
