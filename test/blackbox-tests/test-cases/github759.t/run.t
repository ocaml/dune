  $ opam_prefix="$(opam config var prefix)"
  $ export BUILD_PATH_PREFIX_MAP="/OPAM_PREFIX=$opam_prefix:$BUILD_PATH_PREFIX_MAP"

  $ dune build foo.cma --profile release
  $ dune ocaml-merlin --dump-config=$(pwd)
  Foo
  ((STDLIB /OPAM_PREFIX/lib/ocaml)
   (EXCLUDE_QUERY_DIR)
   (B
    $TESTCASE_ROOT/_build/default/.foo.objs/byte)
   (S
    $TESTCASE_ROOT)
   (FLG (-open Foo -w -40)))

  $ rm -f .merlin
  $ dune build foo.cma --profile release
  $ dune ocaml-merlin --dump-config=$(pwd)
  Foo
  ((STDLIB /OPAM_PREFIX/lib/ocaml)
   (EXCLUDE_QUERY_DIR)
   (B
    $TESTCASE_ROOT/_build/default/.foo.objs/byte)
   (S
    $TESTCASE_ROOT)
   (FLG (-open Foo -w -40)))

  $ echo toto > .merlin
  $ dune build foo.cma --profile release
  $ dune ocaml-merlin --dump-config=$(pwd)
  Foo
  ((STDLIB /OPAM_PREFIX/lib/ocaml)
   (EXCLUDE_QUERY_DIR)
   (B
    $TESTCASE_ROOT/_build/default/.foo.objs/byte)
   (S
    $TESTCASE_ROOT)
   (FLG (-open Foo -w -40)))
