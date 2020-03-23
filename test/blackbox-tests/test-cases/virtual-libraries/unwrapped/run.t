Unwrapped virtual library
  $ dune build
           foo alias default
  Running from vlib_more
  running implementation

Unwrapped virtual library
  $ dune build @install --root vlib
  Entering directory 'vlib'
  $ env OCAMLPATH=vlib/_build/install/default/lib dune build --root impl --debug-dependency-path
  Entering directory 'impl'
           foo alias default
  Running from vlib_more
  running implementation
