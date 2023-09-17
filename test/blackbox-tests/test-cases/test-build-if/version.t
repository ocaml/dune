  $ cat > dune-project << EOF
  > (lang dune 3.8)
  > EOF

  $ cat > dune << EOF
  > (test
  >  (name t)
  >  (build_if true))
  > EOF

  $ touch t.ml

  $ dune build
  File "dune", line 3, characters 1-16:
  3 |  (build_if true))
       ^^^^^^^^^^^^^^^
  Error: 'build_if' is only available since version 3.9 of the dune language.
  Please update your dune-project file to have (lang dune 3.9).
  [1]

  $ cat > dune-project << EOF
  > (lang dune 3.9)
  > EOF

  $ dune build
