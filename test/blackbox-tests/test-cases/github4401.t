When --ignore-promoted-rules is passed, rules marked `(promote (until-clean))`
are ignored. See #4401.

  $ cat > dune-project << EOF
  > (lang dune 3.4)
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

This is correctly ignored if `dune-lang` is bumped to 3.5.

  $ cat > dune-project << EOF
  > (lang dune 3.5)
  > EOF

  $ dune clean
  $ dune runtest --ignore-promoted-rules
