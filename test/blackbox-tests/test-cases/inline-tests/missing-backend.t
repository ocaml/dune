  $ make_dune_project 2.6
  $ cat >dune <<EOF
  > (library
  >  (name foo_mb)
  >  (inline_tests))
  > EOF

  $ dune runtest
  File "dune", line 3, characters 1-15:
  3 |  (inline_tests))
       ^^^^^^^^^^^^^^
  Error: No inline tests backend found.
  [1]
