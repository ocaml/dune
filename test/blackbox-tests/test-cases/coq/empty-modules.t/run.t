We test that the empty modules field in a coq.theory stanza is handled
correctly.

  $ cat > dune << EOF
  > (coq.theory
  >  (name foo)
  >  (modules))
  > EOF

Currently raises an internal excpetion.
  $ dune build 2>&1 | head -n 1
  Error: exception Failure("hd")
