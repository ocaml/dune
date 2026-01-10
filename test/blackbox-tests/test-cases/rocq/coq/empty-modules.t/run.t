We test that the empty modules field in a coq.theory stanza is handled
correctly.

  $ cat > dune << EOF
  > (coq.theory
  >  (name foo)
  >  (modules))
  > EOF

Builds fine as expected.
  $ dune build 2>&1
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
