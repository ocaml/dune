  $ opam_prefix="$(opam var prefix)"
  $ export BUILD_PATH_PREFIX_MAP="/OPAM_PREFIX=$opam_prefix:$BUILD_PATH_PREFIX_MAP"

  $ dune utop react -- init_react.ml
  File "react/react.cma(React)", line 1:
  Warning 31 [module-linked-twice]: files react/react.cma(React) and /OPAM_PREFIX/lib/react/react.cma(React) both define a module named React
  hello in utop
