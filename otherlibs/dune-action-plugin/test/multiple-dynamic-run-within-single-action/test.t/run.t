Check that multiple 'dynamic-run' commands within single action are
detected and error is printed even if the rule is not executed.

  $ cat > dune-project << EOF
  > (lang dune 2.0)
  > (using action-plugin 0.1)
  > EOF

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
  File "dune", lines 4-6, characters 2-84:
  4 |   (progn
  5 |    (dynamic-run ./foo.exe some_arg)
  6 |    (dynamic-run ./foo.exe another_arg))))
  Error: Multiple 'dynamic-run' commands within single action are not
  supported.
  [1]
