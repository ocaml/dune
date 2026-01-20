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

  $ dune build 2>&1 | head -n 5
  File "dune-workspace", line 3, characters 7-29:
  3 |  (test (menhir_flags --table)))
             ^^^^^^^^^^^^^^^^^^^^^^
  Error: 'menhir_flags' is available only when menhir is enabled in the
  dune-project or workspace file. You must enable it using (using menhir 2.1)
  [1]
