  $ ocamlc_where="$(ocamlc -where)"
  $ ENCODED_OCAMLC_WHERE=$(dune_cmd encode-prefix "$ocamlc_where")
  $ export BUILD_PATH_PREFIX_MAP=\
  > "/OCAMLC_WHERE=$ENCODED_OCAMLC_WHERE:$BUILD_PATH_PREFIX_MAP"
  $ for path in $(ocamlfind printconf path)
  > do
  > EPATH=$(dune_cmd encode-prefix "$path")
  > export BUILD_PATH_PREFIX_MAP="/LIB=$EPATH:$BUILD_PATH_PREFIX_MAP"
  > done

CRAM sanitization
  $ dune build ./exe/.merlin-conf/exe-x --profile release
  $ dune ocaml merlin dump-config $PWD/exe
  X
  ((STDLIB /workspace_root/lib/ocaml)
   (EXCLUDE_QUERY_DIR)
   (B /workspace_root/lib/findlib)
   (B /workspace_root/lib/ocaml)
   (B
    $TESTCASE_ROOT/_build/default/exe/.x.eobjs/byte)
   (B
    $TESTCASE_ROOT/_build/default/lib/.foo.objs/public_cmi)
   (S /workspace_root/lib/findlib)
   (S /workspace_root/lib/ocaml)
   (S
    $TESTCASE_ROOT/exe)
   (S
    $TESTCASE_ROOT/lib)
   (FLG
    (-pp
     $TESTCASE_ROOT/_build/default/pp/pp.exe))
   (FLG (-w -40 -g)))

  $ dune build ./lib/.merlin-conf/lib-foo ./lib/.merlin-conf/lib-bar --profile release
  $ dune ocaml merlin dump-config $PWD/lib
  Bar
  ((STDLIB /workspace_root/lib/ocaml)
   (EXCLUDE_QUERY_DIR)
   (B
    $TESTCASE_ROOT/_build/default/lib/.bar.objs/byte)
   (S
    $TESTCASE_ROOT/lib)
   (S
    $TESTCASE_ROOT/lib/subdir)
   (FLG
    (-ppx
     "$TESTCASE_ROOT/_build/default/.ppx/4128e43a9cfb141a37f547484cc9bf46/ppx.exe
     --as-ppx
     --cookie
     'library-name="bar"'"))
   (FLG (-w -40 -g)))
  File
  ((STDLIB /workspace_root/lib/ocaml)
   (EXCLUDE_QUERY_DIR)
   (B
    $TESTCASE_ROOT/_build/default/lib/.bar.objs/byte)
   (S
    $TESTCASE_ROOT/lib)
   (S
    $TESTCASE_ROOT/lib/subdir)
   (FLG (-open Bar))
   (FLG
    (-ppx
     "$TESTCASE_ROOT/_build/default/.ppx/4128e43a9cfb141a37f547484cc9bf46/ppx.exe
     --as-ppx
     --cookie
     'library-name="bar"'"))
   (FLG (-w -40 -g)))
  Foo
  ((STDLIB /workspace_root/lib/ocaml)
   (EXCLUDE_QUERY_DIR)
   (B /workspace_root/lib/findlib)
   (B /workspace_root/lib/ocaml)
   (B
    $TESTCASE_ROOT/_build/default/lib/.foo.objs/byte)
   (S /workspace_root/lib/findlib)
   (S /workspace_root/lib/ocaml)
   (S
    $TESTCASE_ROOT/lib)
   (S
    $TESTCASE_ROOT/lib/subdir)
   (FLG
    (-ppx
     "$TESTCASE_ROOT/_build/default/.ppx/4128e43a9cfb141a37f547484cc9bf46/ppx.exe
     --as-ppx
     --cookie
     'library-name="foo"'"))
   (FLG (-w -40 -g)))
  Privmod
  ((STDLIB /workspace_root/lib/ocaml)
   (EXCLUDE_QUERY_DIR)
   (B /workspace_root/lib/findlib)
   (B /workspace_root/lib/ocaml)
   (B
    $TESTCASE_ROOT/_build/default/lib/.foo.objs/byte)
   (S /workspace_root/lib/findlib)
   (S /workspace_root/lib/ocaml)
   (S
    $TESTCASE_ROOT/lib)
   (S
    $TESTCASE_ROOT/lib/subdir)
   (FLG (-open Foo))
   (FLG
    (-ppx
     "$TESTCASE_ROOT/_build/default/.ppx/4128e43a9cfb141a37f547484cc9bf46/ppx.exe
     --as-ppx
     --cookie
     'library-name="foo"'"))
   (FLG (-w -40 -g)))

Make sure a ppx directive is generated (if not, the [grep ppx] step fails)
  $ dune ocaml merlin dump-config $PWD/lib | grep ppx > /dev/null

Make sure pp flag is correct and variables are expanded

  $ dune build ./pp-with-expand/.merlin-conf/exe-foobar --profile release
  $ dune ocaml merlin dump-config $PWD/pp-with-expand
  Foobar
  ((STDLIB /workspace_root/lib/ocaml)
   (EXCLUDE_QUERY_DIR)
   (B
    $TESTCASE_ROOT/_build/default/pp-with-expand/.foobar.eobjs/byte)
   (S
    $TESTCASE_ROOT/pp-with-expand)
   (FLG
    (-pp
     "$TESTCASE_ROOT/_build/default/pp/pp.exe
     -nothing"))
   (FLG (-w -40 -g)))

Check hash of executables names if more than one
  $ dune build ./exes/.merlin-conf/exe-x-6562915302827c6dce0630390bfa68b7
  $ dune ocaml merlin dump-config $PWD/exes
  X
  ((STDLIB /workspace_root/lib/ocaml)
   (EXCLUDE_QUERY_DIR)
   (B
    $TESTCASE_ROOT/_build/default/exes/.x.eobjs/byte)
   (S
    $TESTCASE_ROOT/exes)
   (FLG
    (-w
     @1..3@5..28@30..39@43@46..47@49..57@61..62-40
     -strict-sequence
     -strict-formats
     -short-paths
     -keep-locs
     -g)))
  Y
  ((STDLIB /workspace_root/lib/ocaml)
   (EXCLUDE_QUERY_DIR)
   (B
    $TESTCASE_ROOT/_build/default/exes/.x.eobjs/byte)
   (S
    $TESTCASE_ROOT/exes)
   (FLG
    (-w
     @1..3@5..28@30..39@43@46..47@49..57@61..62-40
     -strict-sequence
     -strict-formats
     -short-paths
     -keep-locs
     -g)))
