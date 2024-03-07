We test that the empty modules field in a coq.theory stanza is handled
correctly.

  $ cat > dune << EOF
  > (coq.theory
  >  (name foo)
  >  (modules))
  > EOF

Builds fine as expected.
  $ dune build 2>&1
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
