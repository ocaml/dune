copy_files would break the generation of the preprocessing flags
  $ dune build copy_files/.merlin-conf/exe-foo
  $ dune ocaml merlin dump-config $PWD/copy_files |
  > grep -B 1 -A 0 "pp"
  Foo: _build/default/copy_files/foo
  ((INDEX $TESTCASE_ROOT/_build/default/.pp.eobjs/cctx.ocaml-index)
  --
   (S $TESTCASE_ROOT/copy_files/sources)
   (FLG (-pp $TESTCASE_ROOT/_build/default/pp.exe))
  --
  Foo: _build/default/copy_files/foo.ml
  ((INDEX $TESTCASE_ROOT/_build/default/.pp.eobjs/cctx.ocaml-index)
  --
   (S $TESTCASE_ROOT/copy_files/sources)
   (FLG (-pp $TESTCASE_ROOT/_build/default/pp.exe))
