This test demonstrates that -ppx is no more missing when two stanzas are
in the same dune file, but require different ppx specifications

  $ ocamlc_where="$(ocamlc -where)"
  $ export BUILD_PATH_PREFIX_MAP="/OCAMLC_WHERE=$ocamlc_where:$BUILD_PATH_PREFIX_MAP"

  $ dune build @all --profile release
  $ dune ocaml merlin dump-config $PWD
  Usesppx1
  ((STDLIB /OCAMLC_WHERE)
   (EXCLUDE_QUERY_DIR)
   (B
    $TESTCASE_ROOT/_build/default/.usesppx1.objs/byte)
   (S
    $TESTCASE_ROOT)
   (FLG
    (-ppx
     "$TESTCASE_ROOT/_build/default/.ppx/c152d6ca3c7e1d83471ffdf48bf729ae/ppx.exe
     --as-ppx
     --cookie
     'library-name="usesppx1"'"))
   (FLG (-open Usesppx1 -w -40)))
  Usesppx2
  ((STDLIB /OCAMLC_WHERE)
   (EXCLUDE_QUERY_DIR)
   (B
    $TESTCASE_ROOT/_build/default/.usesppx2.objs/byte)
   (S
    $TESTCASE_ROOT)
   (FLG
    (-ppx
     "$TESTCASE_ROOT/_build/default/.ppx/d7394c27c5e0f7ad7ab1110d6b092c05/ppx.exe
     --as-ppx
     --cookie
     'library-name="usesppx2"'"))
   (FLG (-open Usesppx2 -w -40)))
