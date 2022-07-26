When --ignore-promoted-rules is passed, rules marked `(promote (until-clean))`
are ignored. See #4401.

  $ cat > dune-project << EOF
  > (lang dune 3.0)
  > EOF

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
