Testing the composition of the installed stdlib

  $ cat > dune << EOF
  > (coq.theory
  >  (name test)
  >  (theories Coq))
  > EOF

  $ dune build test.vo --display=short --always-show-command-line
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
        coqdep .test.theory.d
          coqc Ntest_test.{cmi,cmxs},test.{glob,vo}
