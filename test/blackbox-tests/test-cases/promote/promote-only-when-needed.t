Test that targets aren't re-promoted if they are up to date.

  $ echo "(lang dune 3.0)" > dune-project
  $ cat >dune <<EOF
  > (rule
  >  (targets promoted)
  >  (mode promote)
  >  (action (with-stdout-to promoted (echo "Hello, world!"))))
  > EOF

  $ dune build promoted --verbose 2>&1 | grep "Promoting"
  Promoting "_build/default/promoted" to "promoted"
  $ cat promoted
  Hello, world!

Dune doesn't promotes the file again if it's unchanged.

  $ dune build promoted --verbose 2>&1 | grep "Promoting"
  [1]
  $ cat promoted
  Hello, world!

Dune does promotes the file again if it's changed.

  $ echo hi > promoted
  $ dune build promoted --verbose 2>&1 | grep "Promoting"
  Promoting "_build/default/promoted" to "promoted"
  $ cat promoted
  Hello, world!
