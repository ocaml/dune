When --ignore-promoted-rules is passed, rules marked `(promote (until-clean))`
are not ignored, independently of dune-lang. See #4401.

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
