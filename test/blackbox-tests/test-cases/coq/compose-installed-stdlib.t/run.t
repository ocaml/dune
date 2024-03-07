Testing the composition of the installed stdlib

  $ cat > dune << EOF
  > (coq.theory
  >  (name test)
  >  (theories Coq))
  > EOF

  $ dune build test.vo --display=short --always-show-command-line
        coqdep .test.theory.d
          coqc Ntest_test.{cmi,cmxs},test.{glob,vo}
