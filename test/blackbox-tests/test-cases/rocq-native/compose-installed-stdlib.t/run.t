Testing the composition of the installed stdlib

  $ cat > dune << EOF
  > (rocq.theory
  >  (name test)
  >  (theories Corelib))
  > EOF

  $ dune build test.vo --display=short --always-show-command-line
          rocq .test.theory.d
          rocq Ntest_test.{cmi,cmxs},test.{glob,vo}
