# Using a ppx in a cross-compiled build context makes dune try to build the ppx
# in the target context instead of the host, then fail.
  $ dune build --debug-dependency-path
  File "lib/dune", line 3, characters 18-24:
  3 |  (preprocess (pps fooppx)))
                        ^^^^^^
  Error: Library "fooppx" in _build/cross-environment/ppx is hidden
  (unsatisfied 'enabled_if').
  -> required by lib/lib.pp.ml (context cross-environment)
  -> required by alias lib/all (context cross-environment)
  -> required by alias default (context cross-environment)
  Hint: try:
    dune external-lib-deps --missing --debug-dependency-path @@default
  [1]
