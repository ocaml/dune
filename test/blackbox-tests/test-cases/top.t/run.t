Basic check that directives are correctly emitted.
  $ dune top
  #directory "$TESTCASE_ROOT/_build/default/.x.objs/byte";;
  #directory "$TESTCASE_ROOT/_build/default/stubs";;
  #directory "$TESTCASE_ROOT/_build/default/stubs/.z.objs/byte";;
  #load "$TESTCASE_ROOT/_build/default/x.cma";;
  #load "$TESTCASE_ROOT/_build/default/stubs/z.cma";;

Check that C stubs work.
  $ (dune top && echo "Z.f ();;") > init.mltop
  $ ocaml init.mltop
  Hello!
