Unwrapped virtual library
  $ dune build
  Running from vlib_more
  running implementation

Unwrapped virtual library
  $ dune build @install --root vlib
  Entering directory 'vlib'
  Leaving directory 'vlib'
  $ env OCAMLPATH=vlib/_build/install/default/lib dune build --root impl --debug-dependency-path
  Entering directory 'impl'
  Running from vlib_more
  running implementation
  Leaving directory 'impl'
