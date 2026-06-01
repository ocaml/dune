When --ignore-promoted-rules is passed, rules marked `(promote (until-clean))`
are ignored. See #4401.

  $ make_dune_project 3.4

  $ echo foobar > reference

  $ cat > dune << EOF
  > (rule
  >  (mode (promote (until-clean)))
  >  (action (with-stdout-to test (run echo foobar))))
  > 
  > (rule
  >  (alias runtest)
  >  (action (diff reference test)))
  > EOF

  $ dune runtest --ignore-promoted-rules
  Error: No rule found for test
  -> required by alias runtest in dune:5
  [1]

This is correctly ignored if `dune-lang` is bumped to 3.5.

  $ make_dune_project 3.5

  $ dune clean
  $ dune runtest --ignore-promoted-rules
