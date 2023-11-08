Dune should not crash when `menhir_flags` are passed in a workspace file.
See #9024.

  $ cat > dune-project << EOF
  > (lang dune 3.0)
  > (using menhir 2.0)
  > EOF

  $ cat > dune-workspace << EOF
  > (lang dune 3.0)
  > (env
  >  (test (menhir_flags --table)))
  > EOF

  $ dune build
  File "dune-workspace", line 3, characters 7-29:
  3 |  (test (menhir_flags --table)))
             ^^^^^^^^^^^^^^^^^^^^^^
  Error: (menhir_flags ...) is not supported in workspace files.
  [1]
