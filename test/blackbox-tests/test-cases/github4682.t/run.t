Attempting to use `(root_module ...)` with an executable that uses PPX results
in an unexpected build failure:

  $ dune build
  Error: Multiple rules generated for _build/default/root.pp.ml-gen:
  - dune:5
  - <none>:1
  [1]
