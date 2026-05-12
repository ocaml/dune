Testing the composition of the installed Corelib

  $ cat > dune << EOF
  > (rocq.theory
  >  (name test))
  > EOF

  $ dune build test.vo --display=short --always-show-command-line
          rocq .test.theory.d
          rocq Ntest_test.{cmi,cmxs},test.{glob,vo}
