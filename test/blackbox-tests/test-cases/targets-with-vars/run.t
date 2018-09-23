  $ dune runtest --root .
  File "dune", line 2, characters 10-23:
  2 |  (targets foo%{ext_obj})
                ^^^^^^^^^^^^^
  Error: Using variables in the targets field is only available since version 1.3 of the dune language
  [1]
