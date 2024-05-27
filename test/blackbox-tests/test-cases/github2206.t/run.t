copy_files would break the generation of the preprocessing flags
  $ dune build copy_files/.merlin-conf/exe-foo
  $ dune ocaml merlin dump-config $PWD/copy_files |
  > grep -B 1 -A 0 "pp"
  ((INDEX
    $TESTCASE_ROOT/_build/default/.pp.eobjs/cctx.ocaml-index)
  --
   (FLG
    (-pp
     $TESTCASE_ROOT/_build/default/pp.exe))
  --
  ((INDEX
    $TESTCASE_ROOT/_build/default/.pp.eobjs/cctx.ocaml-index)
  --
   (FLG
    (-pp
     $TESTCASE_ROOT/_build/default/pp.exe))
