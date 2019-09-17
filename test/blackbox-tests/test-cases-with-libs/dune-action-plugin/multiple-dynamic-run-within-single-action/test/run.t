Check that multiple 'dynamic-run' commands within single action are
detected and error is printed even if the rule is not executed.

  $ echo "(lang dune 2.0)" > dune-project

  $ cat > dune << EOF
  > (rule
  >  (target some_file)
  >  (action
  >   (progn
  >    (dynamic-run ./foo.exe some_arg)
  >    (dynamic-run ./foo.exe another_arg))))
  > \
  > (alias
  >  (name runtest)
  >  (action
  >   (echo "SHOULD NOT BE PRINTED")))
  > EOF

  $ cp ../bin/foo.exe ./

  $ dune runtest
  File "dune", line 4, characters 2-84:
  4 |   (progn
  5 |    (dynamic-run ./foo.exe some_arg)
  6 |    (dynamic-run ./foo.exe another_arg))))
  Error: Multiple 'dynamic-run' commands within single action are not
  supported.
  [1]
