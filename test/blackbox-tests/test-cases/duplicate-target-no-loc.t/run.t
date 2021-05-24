There are 2 rules for a single install file, but the error message doesn't show
us their origin.

Issue: https://github.com/ocaml/dune/issues/1405
  $ dune build foo.install
  Error: Multiple rules generated for _build/install/default/doc/foo/foo:
  - dune:3
  - dune:3
  -> required by _build/default/foo.install
  [1]
