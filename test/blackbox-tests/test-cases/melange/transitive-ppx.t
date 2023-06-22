Test interaction of melange.emit library ppx dependencies

  $ mkdir lib test ppx
  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > (package (name mel-subdir))
  > (using melange 0.1)
  > EOF

  $ mkdir lib/test
  $ touch lib/dune

  $ mkdir lib/impl
  $ cat > lib/impl/dune <<EOF
  > (library
  >  (name subdir)
  >  (modes melange)
  >  (public_name mel-subdir)
  >  (preprocess (pps not-present)))
  > EOF
  $ touch lib/impl/subdir.ml

  $ cat > lib/test/dune <<EOF
  > (melange.emit
  >  (target dist)
  >  (emit_stdlib false)
  >  (modules)
  >  (libraries subdir))
  > EOF

  $ dune build
  File "lib/impl/dune", line 5, characters 18-29:
  5 |  (preprocess (pps not-present)))
                        ^^^^^^^^^^^
  Error: Library "not-present" not found.
  -> required by library "mel-subdir" in _build/default/lib/impl
  -> required by melange target dist
  -> required by alias lib/test/all
  -> required by alias default
  File "lib/impl/dune", line 5, characters 18-29:
  5 |  (preprocess (pps not-present)))
                        ^^^^^^^^^^^
  Error: Library "not-present" not found.
  -> required by melange target dist
  -> required by library "mel-subdir" in _build/default/lib/impl
  -> required by _build/default/lib/test/dist/lib/test/.dist.mobjs/melange.js
  -> required by alias lib/test/all
  -> required by alias default
  [1]
