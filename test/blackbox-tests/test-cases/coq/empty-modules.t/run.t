We test that the empty modules field in a coq.theory stanza is handled
correctly.

  $ cat > dune << EOF
  > (coq.theory
  >  (name foo)
  >  (modules))
  > EOF

Builds fine as expected.
  $ dune build 2>&1
