Dune should warn about duplicate modules declared inside a coq.theory stanza

  $ cat > dune << EOF
  > (coq.theory
  >  (name foo)
  >  (modules foo bar foo))
  > EOF

  $ dune build
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang_lt_08 disabled))
  File "dune", lines 1-3, characters 0-47:
  1 | (coq.theory
  2 |  (name foo)
  3 |  (modules foo bar foo))
  Error: Duplicate Coq module "foo".
  [1]
