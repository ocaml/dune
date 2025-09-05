copy_files would break the generation of the preprocessing flags
  $ dune build copy_files/.merlin-conf/exe-foo
  $ dune ocaml merlin dump-config $PWD/copy_files |
  > grep -B 2 -A 0 "pp"
  Foo: _build/default/copy_files/foo
  ((INDEX $TESTCASE_ROOT/_build/default/.pp.eobjs/cctx.ocaml-index)
  --
   (S $TESTCASE_ROOT/copy_files/sources)
   (FLG (-w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs -g))
   (FLG (-pp $TESTCASE_ROOT/_build/default/pp.exe))
   (UNIT_NAME foo))
  Foo: _build/default/copy_files/foo.ml
  ((INDEX $TESTCASE_ROOT/_build/default/.pp.eobjs/cctx.ocaml-index)
  --
   (S $TESTCASE_ROOT/copy_files/sources)
   (FLG (-w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence -strict-formats -short-paths -keep-locs -g))
   (FLG (-pp $TESTCASE_ROOT/_build/default/pp.exe))
