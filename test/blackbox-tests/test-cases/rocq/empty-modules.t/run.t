We test that the empty modules field in a rocq.theory stanza is handled
correctly.

  $ cat > dune << EOF
  > (rocq.theory
  >  (name foo)
  >  (modules))
  > EOF

Builds fine as expected.
  $ dune build 2>&1
