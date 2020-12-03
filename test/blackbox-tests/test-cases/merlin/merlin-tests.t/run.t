CRAM sanitization
  $ dune build ./exe/.merlin-conf/exe-x --profile release
  $ dune ocaml-merlin --dump-config=$(pwd)/exe |
  > sed 's#'$(opam config var prefix)'#OPAM_PREFIX#'
  x
  ((EXCLUDE_QUERY_DIR)
   (B OPAM_PREFIX/lib/bytes)
   (B OPAM_PREFIX/lib/findlib)
   (B OPAM_PREFIX/lib/ocaml)
   (B
    $TESTCASE_ROOT/_build/default/exe/.x.eobjs/byte)
   (B
    $TESTCASE_ROOT/_build/default/lib/.foo.objs/public_cmi)
   (S OPAM_PREFIX/lib/bytes)
   (S OPAM_PREFIX/lib/findlib)
   (S OPAM_PREFIX/lib/ocaml)
   (S
    $TESTCASE_ROOT/exe)
   (S
    $TESTCASE_ROOT/lib)
   (FLG
    (-pp
     $TESTCASE_ROOT/_build/default/pp/pp.exe))
   (FLG (-w -40)))

  $ dune build ./lib/.merlin-conf/lib-foo ./lib/.merlin-conf/lib-bar --profile release
  $ dune ocaml-merlin --dump-config=$(pwd)/lib |
  > sed 's#'$(opam config var prefix)'#OPAM_PREFIX#'
  bar
  ((EXCLUDE_QUERY_DIR)
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
   (FLG (-open Bar -w -40)))
  file
  ((EXCLUDE_QUERY_DIR)
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
   (FLG (-open Bar -w -40)))
  foo
  ((EXCLUDE_QUERY_DIR)
   (B OPAM_PREFIX/lib/bytes)
   (B OPAM_PREFIX/lib/findlib)
   (B OPAM_PREFIX/lib/ocaml)
   (B
    $TESTCASE_ROOT/_build/default/lib/.foo.objs/byte)
   (S OPAM_PREFIX/lib/bytes)
   (S OPAM_PREFIX/lib/findlib)
   (S OPAM_PREFIX/lib/ocaml)
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
   (FLG (-open Foo -w -40)))
  privmod
  ((EXCLUDE_QUERY_DIR)
   (B OPAM_PREFIX/lib/bytes)
   (B OPAM_PREFIX/lib/findlib)
   (B OPAM_PREFIX/lib/ocaml)
   (B
    $TESTCASE_ROOT/_build/default/lib/.foo.objs/byte)
   (S OPAM_PREFIX/lib/bytes)
   (S OPAM_PREFIX/lib/findlib)
   (S OPAM_PREFIX/lib/ocaml)
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
   (FLG (-open Foo -w -40)))

Make sure a ppx directive is generated
  $ dune ocaml-merlin --dump-config=$(pwd)/lib | grep -q ppx

Make sure pp flag is correct and variables are expanded

  $ dune build ./pp-with-expand/.merlin-conf/exe-foobar --profile release
  $ dune ocaml-merlin --dump-config=$(pwd)/pp-with-expand |
  > sed 's#'$(opam config var prefix)'#OPAM_PREFIX#'
  foobar
  ((EXCLUDE_QUERY_DIR)
   (B
    $TESTCASE_ROOT/_build/default/pp-with-expand/.foobar.eobjs/byte)
   (S
    $TESTCASE_ROOT/pp-with-expand)
   (FLG
    (-pp
     "$TESTCASE_ROOT/_build/default/pp/pp.exe
     -nothing"))
   (FLG (-w -40)))

We want future-syntax to either be applied, or not, depending on OCaml version.
Adding the `echo` with expected output to the set of lines is a way of achieving that.
