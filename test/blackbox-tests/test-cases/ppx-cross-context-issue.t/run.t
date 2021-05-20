# Using a ppx in a cross-compiled build context makes dune try to build the ppx
# in the target context instead of the host, then fail.
  $ dune build --debug-dependency-path
  File "lib/dune", line 3, characters 18-24:
  3 |  (preprocess (pps fooppx)))
                        ^^^^^^
  Error: Library "fooppx" in _build/cross-environment/ppx is hidden
  (unsatisfied 'enabled_if').
  -> required by _build/cross-environment/lib/lib.pp.ml
  -> required by _build/cross-environment/lib/.foolib.objs/lib.pp.ml.d
  -> required by
     _build/cross-environment/lib/.foolib.objs/foolib__Lib.impl.all-deps
  -> required by _build/cross-environment/lib/.foolib.objs/byte/foolib__Lib.cmi
  -> required by
     _build/cross-environment/lib/.foolib.objs/native/foolib__Lib.cmx
  -> required by _build/cross-environment/lib/foolib.a
  -> required by alias lib/all (context cross-environment)
  -> required by alias default (context cross-environment)
  [1]
