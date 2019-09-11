Check that multiple 'dynamic-run' commands within single action are
detected and error is printed even if the rule is not executed.

  $ echo "(lang dune 2.0)" > dune-project

  $ cat > dune << EOF
  > (rule
  >  (target bar)
  >  (action
  >   (progn
  >    (dynamic-run ./foo.exe bar1)
  >    (dynamic-run ./foo.exe bar2))))
  > \
  > (alias
  >  (name runtest)
  >  (action
  >   (echo "SHOULD NOT BE PRINTED")))
  > EOF

  $ cp ../bin/foo.exe ./

  $ dune runtest
  File "dune", line 4, characters 2-73:
  4 |   (progn
  5 |    (dynamic-run ./foo.exe bar1)
  6 |    (dynamic-run ./foo.exe bar2))))
  Error: Multiple 'dynamic-run' commands within single action are not
  supported.
  [1]
