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

Depending on the `subdir` library (preprocessed by a missing PPX) crashes dune

  $ cat > lib/test/dune <<EOF
  > (melange.emit
  >  (target dist)
  >  (emit_stdlib false)
  >  (modules)
  >  (libraries subdir))
  > EOF

  $ dune build 2>&1 | grep "must not crash"
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
